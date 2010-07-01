;;; addressbook-bookmark.el -- An adress book based on Standard Emacs bookmarks.

;; Filename: addressbook-bookmark.el
;; Author: Thierry Volpiatto
;; Maintainer: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Copyright (C) 2009 ~ 2010, Thierry Volpiatto, all rights reserved.

;; Created: <2010-06-19 Sam.>

;; X-URL: http://mercurial.intuxication.org/hg/emacs-bookmark-extension/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;; This is a work in progress, you can use it but it's not finished,
;; so you may find things incoherents or disfunctioning.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(eval-when-compile (require 'cl))
(require 'derived)
(require 'bookmark-extensions)
(require 'message)

(defvar addressbook-anything-complete t
  "*Use anything completion in message buffer.")

(defvar addressbook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'addressbook-quit)
    (define-key map (kbd "m") 'addressbook-set-mail-buffer)
    (define-key map (kbd "C-c C-c") 'addressbook-set-mail-buffer)
    (define-key map (kbd "C-c f c") 'addressbook-set-mail-buffer-and-cc)
    (define-key map (kbd "r") 'addressbook-bookmark-set)
    (define-key map (kbd "C-c g m") 'addressbook-google-map)
    map))

(define-derived-mode addressbook-mode
    text-mode "addressbook"
    "Interface for addressbook.

Special commands:
\\{addressbook-mode-map}")

(defun addressbook-quit ()
  "Quit addressbook buffer."
  (interactive)
  (with-current-buffer "*addressbook*"
    (quit-window)))


(defun addressbook-set-mail-buffer1 (&optional bookmark-name append cc)
  "Setup a mail buffer with BOOKMARK-NAME email using `message-mode'."
  (bookmark-maybe-load-default-file)
  (let ((mail-list ())
        (mail-bufs (message-buffers)))
    (setq mail-list
          (if (eq major-mode 'addressbook-mode)
              (progn
                (forward-line 0)
                (if (search-forward "Mail: " (point-at-eol) t)
                    (progn
                      (skip-chars-forward " " (point-at-eol))
                      (split-string
                       (buffer-substring (point) (point-at-eol)) ", "))
                    (error "Not on a mail entry")))
              (split-string
               (assoc-default
                'email
                (assoc bookmark-name bookmark-alist)) ", ")))
    (cond ((and (or cc append) mail-bufs) ; A mail buffer exists, use it.
           (switch-to-buffer-other-window
            (if (and mail-bufs (> (length mail-bufs) 1))
                (if (fboundp 'anything-comp-read)
                    (anything-comp-read "MailBuffer: " mail-bufs :must-match t)
                    (completing-read "MailBuffer: " mail-bufs nil t))
                (car mail-bufs))))
          ((or cc append)                 ; No mail buffer found create one.
           (compose-mail nil nil nil nil 'switch-to-buffer-other-window))
          (t                              ; create a new mail buffer.
           (compose-mail nil nil nil nil 'switch-to-buffer-other-window)))
    (goto-char (point-min))
    (save-excursion
      (if cc
          (message-goto-cc)
          (or (search-forward "To: " nil t)
              (search-forward "Newsgroups: " nil t)))
      (end-of-line)
      (let ((email (if (> (length mail-list) 1)
                       (if (fboundp 'anything-comp-read)
                           (anything-comp-read
                            "Choose mail: " mail-list :must-match t)
                           (completing-read "Choose mail: " mail-list nil t))
                       (car mail-list))))
        (if append
            (progn
              (message-next-header)
              (forward-line -1)
              (end-of-line)
              (insert (concat ",\n    " email)))
            (insert email))))
    (search-forward "Subject: ")))

(defun addressbook-set-mail-buffer (append)
  "Prepare email buffer with `message-mode' from addressbook buffer."
  (interactive "P")
  (addressbook-set-mail-buffer1 nil append))

(defun addressbook-set-mail-buffer-and-cc (append)
  "Add a cc field to a mail buffer for this bookmark."
  (interactive "P")
  (addressbook-set-mail-buffer1 nil append 'cc))

;;; Completion in message buffer with TAB. (dependency: anything)
(when addressbook-anything-complete
  (require 'anything-config)
  (bookmark-maybe-load-default-file)
  (setq message-tab-body-function 'addressbook-message-complete)
  (setq message-completion-alist
        '(("^\\(Newsgroups\\|Followup-To\\|Posted-To\\|Gcc\\):"
           . addressbook-message-complete)
          ("^\\(Resent-\\)?\\(To\\|B?Cc\\):"
           . addressbook-message-complete)
          ("^\\(Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):"
           . addressbook-message-complete)
          ("^\\(Disposition-Notification-To\\|Return-Receipt-To\\):"
           . addressbook-message-complete)))

  (defun addressbook-message-complete ()
    "Provide addressbook completion for `message-mode'."
    (let* ((ls        (bmkext-addressbook-alist-only))
           (comp-ls   (loop for l in ls
                         collect (cons (car l) (assoc-default 'email l))))
           (cand      (anything-comp-read
                       "Name: " comp-ls
                       :must-match t
                       :initial-input (thing-at-point 'symbol)))
           (cand-list (split-string cand ", ")))
      (end-of-line)
      (while (not (looking-back ": \\|," (point-at-bol))) (delete-char -1))
      (insert (if (> (length cand-list) 1)
                  (anything-comp-read "WhichMail: " cand-list :must-match t)
                  (car cand-list)))
      (goto-char (point-min)) (search-forward "Subject: " nil t))))

(defun addressbook-bookmark-make-entry (name email phone
                                        web street zipcode city image-path &optional nvisit)
  "Build an addressbook bookmark entry."
  `(,name
    ,@(bookmark-make-record-default 'point-only 0 'read-only nvisit)
    (type . "addressbook")
    (location . "Addressbook entry")
    (image . ,image-path)
    (email . ,email)
    (phone . ,phone)
    (web . ,web)
    (street . ,street)
    (zipcode . ,zipcode)
    (city . ,city)
    (handler . addressbook-bookmark-jump)))

(defun addressbook-read-name (prompt)
  "Prompt as many time PROMPT is not empty."
  (let ((var ()))
    (labels ((multiread ()
               (let ((str (read-string prompt))
                     (sep (if (> (length var) 1) ", " "")))
                 (if (string= str "")
                     (mapconcat 'identity (nreverse var) sep)
                     (push str var)
                     (multiread)))))
      (multiread))))


(defun addressbook-bookmark-set ()
  "Record addressbook bookmark entries interactively."
  (interactive)
  (let ((count 0))
    (labels
        ((record ()
           (let ((name       (read-string "Name: "))
                 (email      (addressbook-read-name "Mail: "))
                 (phone      (addressbook-read-name "Phone: "))
                 (web        (addressbook-read-name "Web: "))
                 (street     (read-string "Street: "))
                 (zipcode    (read-string "Zipcode: "))
                 (city       (read-string "City: "))
                 (image-path (read-string "Image path: ")))
               
             (bookmark-maybe-load-default-file)
             (let ((old-entry (assoc name bookmark-alist))
                   (new-entry (addressbook-bookmark-make-entry
                               name email phone web street zipcode city image-path))) 
               (if (and old-entry
                        (string= (assoc-default 'type old-entry) "addressbook"))
                   (setf (cdr old-entry)
                         (cdr (addressbook-bookmark-make-entry
                               name email phone web street zipcode city image-path)))
                   (push new-entry bookmark-alist)))
             (bookmark-bmenu-surreptitiously-rebuild-list)
             (bmkext-maybe-save-bookmark)
             (incf count)
             (if (y-or-n-p (format "`%s' Recorded. Add a new contact? " name))
                 (record)
                 (message "%d Contact(s) added." count)))))
      (record))))

  
(defun addressbook-bookmark-edit (bookmark)
  "Edit an addressbook bookmark entry."
  (let* ((old-name       (car bookmark))
         (old-mail       (assoc-default 'email bookmark))
         (old-phone      (assoc-default 'phone bookmark))
         (old-web        (assoc-default 'web bookmark))
         (old-street     (assoc-default 'street bookmark))
         (old-zipcode    (assoc-default 'zipcode bookmark))
         (old-city       (assoc-default 'city bookmark))
         (old-visit      (assoc-default 'visits bookmark))
         (old-image-path (assoc-default 'image bookmark))
         (name           (read-string "Name: " old-name))
         (mail           (read-string "Mail: " old-mail))
         (phone          (read-string "Phone: " old-phone))
         (web            (read-string "Web: " old-web))
         (street         (read-string "Street: " old-street))
         (zipcode        (read-string "Zipcode: " old-zipcode))
         (city           (read-string "City: " old-city))
         (image-path     (read-string "Image path: " old-image-path))
         (new-entry      (addressbook-bookmark-make-entry
                       name mail phone web street
                       zipcode city image-path old-visit)))
    (when (y-or-n-p "Save changes? ")
      (setcar bookmark name)
      (setcdr bookmark (cdr new-entry))
      (bmkext-maybe-save-bookmark)
      new-entry)))


(defun addressbook-bmenu-edit ()
  "Edit an addresbook bookmark entry from bmenu list."
  (interactive)
  (let* ((name      (bookmark-bmenu-bookmark))
         (bmk       (assoc name bookmark-alist))
         (new-entry (addressbook-bookmark-edit bmk)))
    (when new-entry
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (goto-char (point-min))
      (while (not (string= (car new-entry) (bookmark-bmenu-bookmark)))
        (forward-line 1))
      (forward-line 0)
      (bookmark-bmenu-check-position))))

(defun addressbook-pp-info (name &optional append)
  "Print addressbook entries to an addressbook buffer."
  (bookmark-maybe-load-default-file)
  (let* ((data              (assoc name bookmark-alist))
         (buf               (get-buffer-create "*addressbook*"))
         (mail              (assoc-default 'email data))
         (phone             (assoc-default 'phone data))
         (web               (assoc-default 'web data))
         (street            (assoc-default 'street data))
         (zipcode           (assoc-default 'zipcode data))
         (city              (assoc-default 'city data))
         (image-path        (assoc-default 'image data))
         (image             (unless (or (not image-path)
                                        (string= image-path "")
                                        (not (file-exists-p image-path)))
                              (create-image image-path)))
         (inhibit-read-only t))
    (set-buffer buf)
    (if append
        (goto-char (point-max))
        (erase-buffer) (goto-char (point-min))
        ;; Fixme what is (getenv "USER") on windows system?
        (let ((user (or (getenv "USER") "Unknown user")))
          (insert (propertize (format "Addressbook %s" user)
                              'face '((:foreground "green" :underline t)))
                  "\n\n-----\n")))
    ;; Dont append entry if already there.
    (unless (save-excursion (goto-char (point-min)) (search-forward name nil t))
      (insert (concat (propertize "Name:" 'face '((:underline t)))
                      "    " name))
      (when image (insert-image image))
      (insert "\n"
              (if (string= mail "") ""
                  (concat (propertize "Mail:" 'face '((:underline t)))
                          "    " mail "\n"))
              (if (string= phone "") ""
                  (concat (propertize "Phone:" 'face '((:underline t)))
                          "   " phone "\n"))
              (if (string= web "") ""
                  (concat (propertize "Web:" 'face '((:underline t)))
                          "     " web "\n"))
              (if (string= street "") ""
                  (concat (propertize "Street:" 'face '((:underline t)))
                          "  " street "\n"))
              (if (string= zipcode "") ""
                  (concat (propertize "Zipcode:" 'face '((:underline t)))
                          " " zipcode "\n"))
              (if (string= city "") ""
                  (concat (propertize "City:" 'face '((:underline t)))
                          "    " city "\n"))
              "-----\n")
      (addressbook-mode)
      (setq buffer-read-only t))))

(defun addressbook-get-contact-data ()
  "Get bookmark entry of contact at point in addressbook buffer."
  (with-current-buffer "*addressbook*"
    (search-backward "-----" nil t)
    (search-forward "Name: " nil t)
    (skip-chars-forward " " (point-at-eol))
    (assoc
     (replace-regexp-in-string ; For entry with image.
      " $" "" (buffer-substring (point) (point-at-eol)))
     bookmark-alist)))

(defun addressbook-google-map (&optional bookmark)
  "Show a google map for this address.
This use `google-maps' you can find here:
http://julien.danjou.info/google-maps-el.html."
  (interactive)
  (if (fboundp 'google-maps)
      (let* ((bmk (or bookmark (addressbook-get-contact-data)))
             (street (assoc-default 'street bmk))
             (zipcode (assoc-default 'zipcode bmk))
             (city (assoc-default 'city bmk)))
        (if (not (string= city "")) ; We need at least a city name.
            (google-maps (concat street " " zipcode " " city))
            (message "No address known for this contact")))
      (message "Google maps not available.")))

(defun addressbook-bookmark-jump (bookmark)
  "Default handler to jump to an addressbook bookmark."
  (let ((buf (save-window-excursion
               (if current-prefix-arg
                   (addressbook-pp-info (car bookmark) 'append)
                   (addressbook-pp-info (car bookmark)))
               (current-buffer))))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))


(provide 'addressbook-bookmark)

;;; addressbook-bookmark.el ends here
