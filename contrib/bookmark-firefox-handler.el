;;; bookmark-firefox-handler.el --- 
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
;; 
;;; Commentary:
;;
;; WARNING: use at your own risk
;;
;; It is to bookmark page from firefox to Emacs Bookmarks.
;;
;; It work just like org-annotation-helper.
;;
;; It's a work in process, it is already working here but firefox still
;; difficult to configure in versions > 3, a bug maybe.
;; I will add more info soon ;-)
;;
;; You will find the shell script `emacsbookmark' in this directory.
;;
;; The bookmarklet:
;;
;;-----------------------------------------------------------------------------------------------------------
;; javascript:location.href='emacsbookmark://' + location.href + '::emacsbookmark::' + escape(document.title)
;;-----------------------------------------------------------------------------------------------------------
;;
;; The protocol handlers:(add to your user.js)
;;
;; user_pref("network.protocol-handler.external.emacsbookmark", true);
;; user_pref("network.protocol-handler.app.emacsbookmark", "/home/thierry/bin/emacsbookmark");
;;
;; You will have to hack also the file: mimeTypes.rdf (good luck) due to a bug in firefox-3.*
;;
;; If all went well, you should be able to bookmark a page in firefox with one click on
;; your bookmarklet. :-)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defvar bmkext-firefox-info nil)
(defun bmkext-get-firefox-bmk (bmk)
  (interactive)
  (let* ((split (split-string bmk "::emacsbookmark::"))
         (url   (replace-regexp-in-string "emacsbookmark://" "" (car split)))
         (title (cadr split)))
    (setq url (url-unhex-string url t))
    (setq title (url-unhex-string title t))
    (setq bmkext-firefox-info (cons title url))
    (if (y-or-n-p (format "Really bookmark from Firefox <%s>?" title))
        (progn
          (if (not (member title (bookmark-all-names)))
              (progn
                (bmkext-bookmark-firefox-page bmkext-firefox-info)
                (bmkext-maybe-save-bookmark)
                (call-interactively #'bookmark-bmenu-list)
                (bmkext-bmenu-goto-bookmark title))
              (message "Bookmark <%s> already exists." title)))
        (message "Abort Firefox bookmarking"))))


(defun bmkext-bookmark-firefox-page (bmk)
  (setq bookmark-alist
        (append (list (bmkext-format-w3m-bmk bmkext-firefox-info)) bookmark-alist)))


(provide 'bookmark-firefox-handler)

;;; bookmark-firefox-handler.el ends here
