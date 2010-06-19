;;; adressbook-bookmark.el -- An adress book based on Standard Emacs bookmarks.

;; Filename: adressbook-bookmark.el
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

(defvar adressbook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'adressbook-quit)
    (define-key map (kbd "m") 'adressbook-set-mail-buffer)
    (define-key map (kbd "s") 'adressbook-write)
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

(defun adressbook-bookmark-make-entry (name email phone)
  `(,name
    ,@(bookmark-make-record-default 'point-only 0 'read-only)
    (type . "adressbook")
    (email . ,email)
    (phone . ,phone)
    (handler . adressbook-bookmark-jump)))

(defun adressbook-read-name (prompt)
  "Prompt as many time PROMPT is not empty."
  (let ((var ()))
    (labels ((multiread ()
               (let ((str (read-string prompt)))
                 (if (string= str "")
                     (mapconcat 'identity (nreverse var) " ")
                     (push str var)
                     (multiread)))))
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
        (push new-entry adressbook-bookmark-alist)))
  (adressbook-set-save-flag))

(defvar adressbook-modification-flag 0)
(add-hook 'bookmark-exit-hook 'adressbook-bookmark-save)
(defun adressbook-bookmark-save ()
  (and adressbook-bookmark-alist
       (> adressbook-modification-flag 0)
       (adressbook-write)))

(defun adressbook-set-save-flag ()
  (setq adressbook-modification-flag (1+ adressbook-modification-flag)))
  
(defun adressbook-bookmark-edit (bookmark)
  (let ((name (read-string "Name: " (car bookmark)))
        (mail (read-string "Mail: " (assoc-default 'email bookmark)))
        (phone (read-string "Phone: " (assoc-default 'phone bookmark))))
    (setf (cdr bookmark) (cdr (adressbook-bookmark-make-entry name mail phone)))
    (adressbook-set-save-flag)))

(defun adressbook-bmenu-edit ()
  (interactive)
  (let ((bmk (assoc (bookmark-bmenu-bookmark) adressbook-bookmark-alist)))
    (adressbook-bookmark-edit bmk)))

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
