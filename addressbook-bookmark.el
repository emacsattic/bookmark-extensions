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
    (define-key map (kbd "r") 'addressbook-bookmark-set)
    map))

(define-derived-mode addressbook-mode
    text-mode "addressbook"
    "Interface for addressbook.

Special commands:
\\{addressbook-mode-map}"
    (kill-all-local-variables)
    (use-local-map addressbook-mode-map)    
    (setq buffer-read-only t))


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

(defun addressbook-set-mail-buffer (&optional append)
  (interactive "P")
  (let ((mail-list ())
        (mail-bufs (message-buffers)))
    (forward-line 0)
    (if (search-forward "Mail: " (point-at-eol) t)
        (setq mail-list (split-string (buffer-substring (point) (point-at-eol))))
        (error "Not on a mail entry"))
    (if (or append mail-bufs)
        (switch-to-buffer-other-window
         (if (and mail-bufs (> (length mail-bufs) 1))
             (anything-comp-read "MailBuffer: " mail-bufs :must-match t)
             (car mail-bufs)))
        (compose-mail nil nil nil nil 'switch-to-buffer-other-window))
    (goto-char (point-min))
    (save-excursion
      (or (search-forward "To: " nil t)
          (search-forward "Newsgroups: " nil t))
      (end-of-line)
      (let ((email (if (> (length mail-list) 1)
                       (anything-comp-read "Choose mail: " mail-list :must-match t)
                       (car mail-list))))
        (if append (insert (concat ", " email)) (insert email))))
    (search-forward "Subject: ")))

(defun addressbook-bookmark-make-entry (name email phone)
  `(,name
    ,@(bookmark-make-record-default 'point-only 0 'read-only)
    (type . "addressbook")
    (location . "Addressbook entry")
    (email . ,email)
    (phone . ,phone)
    (handler . addressbook-bookmark-jump)))

(defun addressbook-read-name (prompt)
  "Prompt as many time PROMPT is not empty."
  (let ((var ()))
    (labels ((multiread ()
               (let ((str (read-string prompt)))
                 (if (string= str "")
                     (mapconcat 'identity (nreverse var) " ")
                     (push str var)
                     (multiread)))))
      (multiread))))

(defun addressbook-bookmark-set (name email phone)
  (interactive (list (read-string "Name: ")
                     (addressbook-read-name "Mail: ")
                     (addressbook-read-name "Phone: ")))
  (bookmark-maybe-load-default-file)
  (let ((old-entry (assoc name bookmark-alist))
        (new-entry (addressbook-bookmark-make-entry name email phone))) 
    (if (and old-entry (string= (assoc-default 'type old-entry) "addressbook"))
        (setf (cdr old-entry) (cdr (addressbook-bookmark-make-entry name email phone)))
        (push new-entry bookmark-alist)))
  (bookmark-bmenu-surreptitiously-rebuild-list)
  (bmkext-maybe-save-bookmark))

  
(defun addressbook-bookmark-edit (bookmark)
  (let* ((old-name  (car bookmark))
         (old-mail  (assoc-default 'email bookmark))
         (old-phone (assoc-default 'phone bookmark))
         (name      (read-string "Name: " old-name))
         (mail      (read-string "Mail: " old-mail))
         (phone     (read-string "Phone: " old-phone))
         (new-entry (addressbook-bookmark-make-entry name mail phone)))
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
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (goto-char (point-min))
    (while (not (string= (car new-entry) (bookmark-bmenu-bookmark)))
      (forward-line 1))
    (forward-line 0)
    (bookmark-bmenu-check-position)))

(defun addressbook-pp-info (name &optional append)
  (let ((data (assoc name bookmark-alist))
        (buf  (get-buffer-create "*addressbook*"))
        (inhibit-read-only t))
    (set-buffer buf)
    (if append
        (goto-char (point-max))
        (erase-buffer) (goto-char (point-min)))
    (insert (concat "Name: " name "\n")
            (concat "Mail: " (assoc-default 'email data) "\n")
            (concat "Phone: " (assoc-default 'phone data) "\n-----\n"))
    (addressbook-mode)))

(defun addressbook-bookmark-jump (bookmark)
  (let ((buf (save-window-excursion
               (if current-prefix-arg
                   (addressbook-pp-info (car bookmark) 'append)
                   (addressbook-pp-info (car bookmark)))
               (current-buffer))))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))


(provide 'addressbook-bookmark)

;; FIXME
;; use real bookmark-alist or not?
;; in this case i need to create filter to show only addressbook entry (type).

;; TODO
;; methods to prepare mail buffer (from bmenu-list and addressbook buffer)==> Yes
;; handle multiples mails for same contact                               ==> Yes
;; enable append to addressbook buffer                                    ==> Yes
;; make use of marked bookmarks
