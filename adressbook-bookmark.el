;;; adressbook-bookmark.el -- An adress book based on Standard Emacs bookmarks.

;;; Code:
(eval-when-compile (require 'cl))
(require 'derived)
(require 'bookmark-extensions)

(defvar adressbook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'adressbook-quit)
    (define-key map (kbd "m") 'adressbook-set-mail-buffer)
    ;(define-key map (kbd "e") 'adressbook-edit-line)
    map))

(define-derived-mode adressbook-mode
    text-mode "adressbook"
    "Interface for adressbook.

Special commands:
\\{adressbook-mode-map}")

(defvar adressbook-default-file "~/.adressbook.bmk")
(defvar adressbook-bookmark-alist nil)

(defun adressbook-quit ()
  (interactive)
  (with-current-buffer "*adressbook*"
    (quit-window)))

;; (defun adressbook-message-buffer
;; (let (buffers)
;;   (save-excursion
;;     (dolist (buffer (buffer-list t))
;;       (set-buffer buffer)
;;       (when (eq major-mode 'mail-mode)
;;         (push (buffer-name buffer) buffers))))
;;   (nreverse buffers)))

;; Use ==> (message-buffers) to get mail/news buffers.

(defun adressbook-set-mail-buffer (&optional append)
  (interactive "P")
  (let (mail)
    (forward-line 0)
    (if (search-forward "Mail: " (point-at-eol) t)
        (setq mail (split-string (buffer-substring (point) (point-at-eol))))
        (error "Not on a mail entry"))
    (if (or append (get-buffer "*mail*"))
        (switch-to-buffer-other-window "*mail*")
        (compose-mail nil nil nil nil 'switch-to-buffer-other-window))
    (goto-char (point-min))
    (save-excursion
      (search-forward "To: ") (end-of-line)
      (let ((email (if (> (length mail) 1)
                       (anything-comp-read "Choose mail: " mail :must-match t)
                       (car mail))))
        (if append (insert (concat ", " email)) (insert email))))
    (search-forward "Subject: ")))

(defun adressbook-bookmark-make-entry (name email phone)
  `(,name
    ,@(bookmark-make-record-default 'point-only 0 'read-only)
    (type . "adressbook")
    (email . ,email)
    (phone . ,phone)
    (handler . adressbook-bookmark-jump)))

(defun adressbook-read-name (prompt)
  "Prompt as many time you add + to end of PROMPT."
  (let (var)
    (labels ((multiread ()
               (let* ((str   (read-string (format "%s(add `+' to repeat): " prompt)))
                      (stock (replace-regexp-in-string "\+$" "" str)))
                 (cond ((string-match "\+$" str)
                        (push stock var)
                        (multiread))
                       (t
                        (push stock var)
                        (mapconcat 'identity (nreverse var) " "))))))
      (multiread))))

(defun adressbook-bookmark-set (name email phone)
  (interactive (list (read-string "Name: ")
                     (adressbook-read-name "Mail: ")
                     (adressbook-read-name "Phone: ")))
  (adressbook-maybe-load-file)
  (let ((old-entry (assoc name adressbook-bookmark-alist))
        (new-entry (adressbook-bookmark-make-entry name email phone))) 
    (if (and old-entry (string= (assoc-default 'type old-entry) "adressbook"))
        (setf (cdr old-entry) (cdr (adressbook-bookmark-make-entry name email phone)))
        (push new-entry adressbook-bookmark-alist))))


(defun adressbook-pp-info (name &optional append)
  (let ((data (assoc name adressbook-bookmark-alist))
        (buf  (get-buffer-create "*adressbook*")))
    (set-buffer buf)
    (if append
        (goto-char (point-max))
        (erase-buffer) (goto-char (point-min)))
    (adressbook-mode)
    (insert (concat "Name: " name "\n")
            (concat "Mail: " (assoc-default 'email data) "\n")
            (concat "Phone: " (assoc-default 'phone data) "\n-----\n"))))

(defun adressbook-bookmark-jump (bookmark)
  (let ((buf (save-window-excursion
               (if current-prefix-arg
                   (adressbook-pp-info (car bookmark) 'append)
                   (adressbook-pp-info (car bookmark)))
               (current-buffer))))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

(defun adressbook-write ()
  (interactive)
  (adressbook-maybe-load-file)
  (let ((bookmark-alist adressbook-bookmark-alist))
    (bookmark-save nil adressbook-default-file)))

(defun adressbook-maybe-load-file (&optional reload)
  (with-current-buffer (find-file-noselect adressbook-default-file)
    (unless (or reload adressbook-bookmark-alist)
      (setq adressbook-bookmark-alist (bookmark-alist-from-buffer)))))
    
(defun adressbook-bmenu-list ()
  (interactive)
  (let (bookmark-alist)
    (adressbook-maybe-load-file)
    (setq bookmark-alist adressbook-bookmark-alist)
    (bookmark-bmenu-list "% AdressBook")
    (switch-to-buffer "*Bookmark List*")))

(provide 'adressbook-bookmark)

;; FIXME
;; use real bookmark-alist or not?
;; in this case i need to create filter to show only adressbook entry (type).

;; TODO
;; methods to prepare mail buffer (from bmenu-list and adressbook buffer)
;; handle multiples mails for same contact
;; enable append to adressbook buffer
;; make use of marked bookmarks
