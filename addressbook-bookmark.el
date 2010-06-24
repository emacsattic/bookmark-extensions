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

(defvar addressbook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'addressbook-quit)
    (define-key map (kbd "m") 'addressbook-set-mail-buffer)
    (define-key map (kbd "C-c C-c") 'addressbook-set-mail-buffer)
    (define-key map (kbd "C-c f c") 'addressbook-set-mail-buffer-and-cc)
    (define-key map (kbd "r") 'addressbook-bookmark-set)
    map))

(define-derived-mode addressbook-mode
    text-mode "addressbook"
    "Interface for addressbook.

Special commands:
\\{addressbook-mode-map}")

(defun addressbook-quit ()
  (interactive)
  (with-current-buffer "*addressbook*"
    (quit-window)))

;; (defun addressbook-message-buffer
;; (let (buffers)
;;   (save-excursion
;;     (dolist (buffer (buffer-list t))
;;       (set-buffer buffer)
;;       (when (eq major-mode 'mail-mode)
;;         (push (buffer-name buffer) buffers))))
;;   (nreverse buffers)))

;; Use ==> (message-buffers) to get mail/news buffers.

(defun addressbook-set-mail-buffer1 (&optional append cc)
  (let ((mail-list ())
        (mail-bufs (message-buffers)))
    (setq mail-list
          (cond ((eq major-mode 'addressbook-mode)
                 (progn
                   (forward-line 0)
                   (if (search-forward "Mail: " (point-at-eol) t)
                       (split-string
                        (buffer-substring (point) (point-at-eol)) ", ")
                       (error "Not on a mail entry"))))
                ((eq major-mode 'bookmark-bmenu-mode)
                 (split-string
                  (assoc-default
                   'email
                   (assoc (bookmark-bmenu-bookmark) bookmark-alist)) ", "))
                (t (error "Command not available from here"))))
    (cond ((and (or cc append) mail-bufs) ; A mail buffer exists, use it.
           (switch-to-buffer-other-window
            (if (and mail-bufs (> (length mail-bufs) 1))
                (anything-comp-read "MailBuffer: " mail-bufs :must-match t)
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
                       (anything-comp-read "Choose mail: "
                                           mail-list :must-match t)
                       (car mail-list))))
        (if append (insert (concat ", " email)) (insert email))))
    (search-forward "Subject: ")))

(defun addressbook-set-mail-buffer (append)
  (interactive "P")
  (addressbook-set-mail-buffer1 append))

(defun addressbook-set-mail-buffer-and-cc (append)
  (interactive "P")
  (addressbook-set-mail-buffer1 append 'cc))

(defun addressbook-bookmark-make-entry (name email phone
                                        web street zipcode city)
  `(,name
    ,@(bookmark-make-record-default 'point-only 0 'read-only)
    (type . "addressbook")
    (location . "Addressbook entry")
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
  (interactive)
  (let ((count   0))
    (labels
        ((record ()
           (let ((name    (read-string "Name: "))
                 (email   (addressbook-read-name "Mail: "))
                 (phone   (addressbook-read-name "Phone: "))
                 (web     (addressbook-read-name "Web: "))
                 (street  (read-string "Street: "))
                 (zipcode (read-string "Zipcode: "))
                 (city    (read-string "City: ")))
               
             (bookmark-maybe-load-default-file)
             (let ((old-entry (assoc name bookmark-alist))
                   (new-entry (addressbook-bookmark-make-entry
                               name email phone web street zipcode city))) 
               (if (and old-entry (string= (assoc-default 'type old-entry) "addressbook"))
                   (setf (cdr old-entry)
                         (cdr (addressbook-bookmark-make-entry
                               name email phone web street zipcode city)))
                   (push new-entry bookmark-alist)))
             (bookmark-bmenu-surreptitiously-rebuild-list)
             (bmkext-maybe-save-bookmark)
             (incf count)
             (if (y-or-n-p (format "`%s' Recorded. Add a new contact? " name))
                 (record)
                 (message "%d Contact(s) added." count)))))
      (record))))

  
(defun addressbook-bookmark-edit (bookmark)
  (let* ((old-name    (car bookmark))
         (old-mail    (assoc-default 'email bookmark))
         (old-phone   (assoc-default 'phone bookmark))
         (old-street  (assoc-default 'street bookmark))
         (old-zipcode (assoc-default 'zipcode bookmark))
         (old-city    (assoc-default 'city bookmark))
         (name        (read-string "Name: " old-name))
         (mail        (read-string "Mail: " old-mail))
         (phone       (read-string "Phone: " old-phone))
         (web         (read-string "Web: " old-phone))
         (street      (read-string "Street: " old-street))
         (zipcode     (read-string "Zipcode: " old-zipcode))
         (city        (read-string "City: " old-city))
         (new-entry   (addressbook-bookmark-make-entry
                     name mail phone web street zipcode city)))
    (when (y-or-n-p "Save changes? ")
      (setcar bookmark name)
      (setcdr bookmark (cdr new-entry))
      (bmkext-maybe-save-bookmark)
      new-entry)))


(defun addressbook-bmenu-edit ()
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
  (let ((data (assoc name bookmark-alist))
        (buf  (get-buffer-create "*addressbook*"))
        (inhibit-read-only t))
    (set-buffer buf)
    (if append
        (goto-char (point-max))
        (erase-buffer) (goto-char (point-min)))
    (insert (concat (propertize "Name:" 'face '((:underline t)))
                    "    " name "\n")
            (concat (propertize "Mail:" 'face '((:underline t)))
                    "    " (assoc-default 'email data) "\n")
            (concat (propertize "Phone:" 'face '((:underline t)))
                    "   " (assoc-default 'phone data) "\n")
            (concat (propertize "Web:" 'face '((:underline t)))
                    "     " (assoc-default 'web data) "\n")
            (concat (propertize "Street:" 'face '((:underline t)))
                    "  " (assoc-default 'street data) "\n")
            (concat (propertize "Zipcode:" 'face '((:underline t)))
                    " " (assoc-default 'zipcode data) "\n")
            (concat (propertize "City:" 'face '((:underline t)))
                    "    " (assoc-default 'city data) "\n-----\n"))
    (addressbook-mode)
    (setq buffer-read-only t)))

(defun addressbook-bookmark-jump (bookmark)
  (let ((buf (save-window-excursion
               (if current-prefix-arg
                   (addressbook-pp-info (car bookmark) 'append)
                   (addressbook-pp-info (car bookmark)))
               (current-buffer))))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))


(provide 'addressbook-bookmark)

;;; addressbook-bookmark.el ends here
