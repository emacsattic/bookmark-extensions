;;; icicle-bookmark-region.el --- 
;; 
;; Author: 
;; Maintainer: 
;; 
;; Created: ven. juin 12 15:43:59 2009 (+0200)
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
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defun icicle-exchange-point-and-mark (&optional arg) ; Bound to `C-x C-x'.
  "`exchange-point-and-mark', `icicle-add-region', or `icicle-select-region'.
With no prefix arg: `exchange-point-and-mark'.
With a numeric prefix arg:`icicle-add-region'.
With a plain `C-u' prefix arg: `icicle-select-region'.

By default, Icicle mode remaps all key sequences that are normally
bound to `exchange-point-and-mark' to
`icicle-exchange-point-and-mark'.  If you do not want this remapping,
then customize option `icicle-top-level-key-bindings'."
  (interactive "P")
  (if arg
      (if (atom arg)
          (call-interactively #'icicle-bookmark-cmd 1) ;(call-interactively #'icicle-add-region)
        (unless (consp (bookmark-region-alist-only));icicle-region-alist)
          (error "`icicle-region-alist' is empty; try again, with a numeric prefix arg"))
        (call-interactively #'icicle-select-region))
    (call-interactively #'exchange-point-and-mark)))

(icicle-define-command icicle-bookmark  ; Command name
  "Jump to a bookmark.
You can use `S-delete' on any bookmark during completion to delete it.
If `crosshairs.el' is loaded, then the target position is highlighted." ; Doc string
  icicle-bookmark-jump                  ; Function to perform the action
  "Bookmark: " (mapcar #'list (if current-prefix-arg
                                  (bookmark-region-alist-only-names)
                                  (bookmark-all-names))) ; `completing-read' args
  nil t nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (and (boundp 'bookmark-current-bookmark) bookmark-current-bookmark)
  nil
  ((completion-ignore-case          bookmark-completion-ignore-case) ; Additional bindings
   (icicle-delete-candidate-object  'bookmark-delete))
  nil (icicle-bookmark-cleanup) (icicle-bookmark-cleanup)) ; First code, undo code, last code

(icicle-define-command icicle-select-region  ; Command name
  "Jump to a bookmark.
You can use `S-delete' on any bookmark during completion to delete it.
If `crosshairs.el' is loaded, then the target position is highlighted." ; Doc string
  icicle-bookmark-jump                  ; Function to perform the action
  "Bookmark: " (mapcar #'list (bookmark-region-alist-only-names)) ; `completing-read' args
  nil t nil (if (boundp 'bookmark-history) 'bookmark-history 'icicle-bookmark-history)
  (and (boundp 'bookmark-current-bookmark) bookmark-current-bookmark)
  nil
  ((completion-ignore-case          bookmark-completion-ignore-case) ; Additional bindings
   (icicle-delete-candidate-object  'bookmark-delete))
  nil (icicle-bookmark-cleanup) (icicle-bookmark-cleanup)) ; First code, undo code, last code

(defun icicle-bookmark-jump (bookmark)
  "Jump to BOOKMARK.
You probably don't want to use this.  Use `icicle-bookmark' instead.
If `crosshairs.el' is loaded, then the target position is highlighted."
  (interactive (list (bookmark-completing-read "Jump to bookmark" bookmark-current-bookmark)))
  (bookmark-jump-other-window bookmark))
  ;(icicle-bookmark-jump-1 bookmark))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicle-bookmark-region.el ends here
