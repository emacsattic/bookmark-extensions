;;; bookmark-uzbl-handler.el --- 
;; 
;; Author: 
;; Maintainer: 
;; 
;; Created: <2009-11-17 Mar. 21:35>
;; Version: 
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'url)
(autoload 'url-unhex-string "url")


(defun bmkext-get-uzbl-bmk (title url)
  "Bookmark a Uzbl page in Standards Emacs bookmarks."
  (interactive)
  (setq url (url-unhex-string url t))
  (setq title (url-unhex-string title t))
  (let ((bmkext-uzbl-info (cons title url)))
    (if (and title url (y-or-n-p (format "Bookmark (%s) from uzbl? " title)))
        (progn
          (if (not (member title (bookmark-all-names)))
              (progn
                (setq bookmark-alist
                      (bmkext-bookmark-uzbl-page bmkext-uzbl-info))
                (bmkext-maybe-save-bookmark)
                (call-interactively #'bookmark-bmenu-list)
                (sit-for 2)
                (bmkext-bmenu-goto-bookmark title))
              (message "Bookmark (%s) already exists." title)))
        (message "Abort Uzbl bookmarking"))))

(defun* bmkext-bookmark-uzbl-page (bmk)
  "Return `bookmark-alist' with the firefox bookmark BMK appended to it."
  (append
   (list (bmkext-format-html-bmk bmk "uzbl-record"))
   bookmark-alist))

(provide 'bookmark-uzbl-handler)
