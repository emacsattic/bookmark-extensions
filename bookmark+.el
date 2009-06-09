;;; bookmark+.el --- Extensions to `bookmark.el'.
;;
;; Filename: bookmark+.el
;; Description: Extensions to `bookmark.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2009, Drew Adams, all rights reserved.
;; Created: Fri Sep 15 07:58:41 2000
;; Version: 21.0
;; Last-Updated: Mon May 25 12:37:46 2009 (-0700)
;;           By: dradams
;;     Update #: 167
;; URL: http://www.emacswiki.org/cgi-bin/wiki/bookmark+.el
;; Keywords: bookmarks, placeholders, annotations, search
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `bookmark', `pp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to `bookmark.el'.
;;
;;  Commands defined here:
;;
;;   `bookmark-jump-other-window' (Emacs 20-22),
;;   `bookmark-menu-jump-other-window' (Emacs 20-21).
;;
;;
;;  ***** NOTE: The following functions defined in `bookmark.el'
;;              have been REDEFINED HERE:
;;
;;   `bookmark-completing-read', `bookmark-delete',
;;   `bookmark-get-bookmark-record', `bookmark-insert',
;;   `bookmark-insert-location', `bookmark-jump', `bookmark-relocate',
;;   `bookmark-rename'.
;;
;;  These functions (except `bookmark-get-bookmark-record') all bind
;;  `S-delete' to delete the current bookmark candidate during
;;  completion, in Icicle mode (see Icicles:
;;  http://www.emacswiki.org/cgi-bin/wiki/Icicles).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2009/05/25 dadams
;;     Added redefinition of bookmark-get-bookmark-record.
;; 2008/10/16 dadams
;;     bookmark-jump-other-window: Don't define it for Emacs 23+ (not needed).
;; 2008/04/04 dadams
;;     bookmark-jump-other-window: Updated wrt Emacs 22.2.
;; 2007/10/07 dadams
;;     Added: bookmark-completing-read, bookmark-delete, bookmark-insert(-location),
;;            bookmark-jump, bookmark-relocate, bookmark-rename.
;;     bookmark-jump-other-window: Use new bookmark-completing-read.
;; 2007/07/13 dadams
;;     Replaced Emacs version tests to reflect Emacs 22 release.
;; 2006/03/08 dadams
;;     bookmark-jump-other-window: Handle nil arg.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/10/26 dadams
;;     Different menu-bar command, depending on Emacs version.
;; 2004/09/21 dadams
;;     Only define bookmark-menu-jump-other-window if < Emacs 21.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'bookmark)


;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-key bookmark-map "o" 'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "q" 'bookmark-jump-other-window)
;;;###autoload
(define-key ctl-x-map "p" bookmark-map)
;;;###autoload
(define-key ctl-x-map "pj" 'bookmark-jump-other-window)



;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Binds `icicle-delete-candidate-object' to `bookmark-delete'.
;;
(defun bookmark-completing-read (prompt &optional default)
  "Prompting with PROMPT, read a bookmark name in completion.
PROMPT will get a \": \" stuck on the end no matter what, so you
probably don't want to include one yourself.
Optional second arg DEFAULT is a string to return if the user enters
the empty string.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (bookmark-maybe-load-default-file)    ; paranoia
  (if (listp last-nonmenu-event)
      (bookmark-menu-popup-paned-menu t prompt (bookmark-all-names))
    (let* ((icicle-delete-candidate-object 'bookmark-delete) ; For `S-delete'.
           (completion-ignore-case bookmark-completion-ignore-case)
           (default default)
           (prompt (if default
                       (concat prompt (format " (%s): " default))
                     (concat prompt ": ")))
           (str (completing-read prompt bookmark-alist nil 0 nil 'bookmark-history)))
      (if (string-equal "" str) default str))))

;;;###autoload
(if (> emacs-major-version 21)
    (define-key-after menu-bar-bookmark-map [jump-other]
      '("Jump to Bookmark (Other Window)" . bookmark-jump-other-window)
      'jump)
  (define-key-after menu-bar-bookmark-map [jump-other]
    '("Jump to Bookmark (Other Window)" . bookmark-menu-jump-other-window)
    'jump))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Adds note about `S-delete' to doc string.
;;
(or (fboundp 'old-bookmark-relocate)
(fset 'old-bookmark-relocate (symbol-function 'bookmark-relocate)))

;;;###autoload
(defun bookmark-relocate (bookmark)
  "Relocate BOOKMARK to another file (reading file name with minibuffer).
This makes an already existing bookmark point to that file, instead of
the one it used to point at.  Useful when a file has been renamed
after a bookmark was set in it.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Bookmark to relocate")))
  (old-bookmark-relocate bookmark))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Adds note about `S-delete' to doc string.
;;
(or (fboundp 'old-bookmark-insert-location)
(fset 'old-bookmark-insert-location (symbol-function 'bookmark-insert-location)))

;;;###autoload
(defun bookmark-insert-location (bookmark &optional no-history)
  "Insert the name of the file associated with BOOKMARK.
Optional second arg NO-HISTORY means don't record this in the
minibuffer history list `bookmark-history'.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Insert bookmark location")))
  (old-bookmark-insert-location bookmark no-history))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Adds note about `S-delete' to doc string.
;;
(or (fboundp 'old-bookmark-rename)
(fset 'old-bookmark-rename (symbol-function 'bookmark-rename)))

;;;###autoload
(defun bookmark-rename (old &optional new)
  "Change the name of OLD bookmark to NEW name.
If called from keyboard, prompt for OLD and NEW.  If called from
menubar, select OLD from a menu and prompt for NEW.

If called from Lisp, prompt for NEW if only OLD was passed as an
argument.  If called with two strings, then no prompting is done.  You
must pass at least OLD when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consecutive words from the text of the buffer into the new bookmark
name.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Old bookmark name")))
  (old-bookmark-rename old new))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Adds note about `S-delete' to doc string.
;;
(or (fboundp 'old-bookmark-insert)
(fset 'old-bookmark-insert (symbol-function 'bookmark-insert)))

;;;###autoload
(defun bookmark-insert (bookmark)
  "Insert the text of the file pointed to by bookmark BOOKMARK.
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Insert bookmark contents")))
  (old-bookmark-insert bookmark))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Lets older versions of Emacs handle bookmarks created with Emacs 23.
;; This is just the Emacs 23 definition of the function.
;;
(when (< emacs-major-version 23)
  (or (fboundp 'old-bookmark-get-bookmark-record)
(fset 'old-bookmark-get-bookmark-record
      (symbol-function 'bookmark-get-bookmark-record)))

;;;###autoload
  (defun bookmark-get-bookmark-record (bookmark)
    "Return the guts of the entry for BOOKMARK in `bookmark-alist'.
That is, all information but the name."
    (let ((alist (cdr (bookmark-get-bookmark bookmark))))
      ;; The bookmark objects can either look like (NAME ALIST) or
      ;; (NAME . ALIST), so we have to distinguish the two here.
      (if (and (null (cdr alist)) (consp (caar alist)))
          (car alist) alist))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Adds note about `S-delete' to doc string.
;;
(or (fboundp 'old-bookmark-delete)
(fset 'old-bookmark-delete (symbol-function 'bookmark-delete)))

;;;###autoload
(defun bookmark-delete (bookmark &optional batch)
  "Delete BOOKMARK from the bookmark list.
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark \(that is, the
one most recently used in this file, if any\).
Optional second arg BATCH means don't update the bookmark list buffer,
probably because we were called from there.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate.  In this way, you can delete multiple bookmarks."
  (interactive (list (bookmark-completing-read "Delete bookmark"
                                               bookmark-current-bookmark)))
  (old-bookmark-delete bookmark batch))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Adds note about `S-delete' to doc string.
;;
(or (fboundp 'old-bookmark-jump)
(fset 'old-bookmark-jump (symbol-function 'bookmark-jump)))

;;;###autoload
(defun bookmark-jump (bookmark)
  "Jump to bookmark BOOKMARK (a point in some file).
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

If the file pointed to by BOOKMARK no longer exists, you will be asked
if you wish to give the bookmark a new location, and `bookmark-jump'
will then jump to the new location, as well as recording it in place
of the old one in the permanent bookmark record.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Jump to bookmark"
                                               bookmark-current-bookmark)))
  (old-bookmark-jump bookmark))

;;;###autoload
(unless (> emacs-major-version 22)
  (defun bookmark-jump-other-window (bookmark)
    "Jump to BOOKMARK (a point in some file) in another window.
See `bookmark-jump'.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
    (interactive (list (bookmark-completing-read
                        "Jump to bookmark (in another window)"
                        bookmark-current-bookmark)))
    (unless bookmark (error "No bookmark specified"))
    (bookmark-maybe-historicize-string bookmark)
    (let ((cell (bookmark-jump-noselect bookmark)))
      (and cell
           (switch-to-buffer-other-window (car cell))
           (goto-char (cdr cell))
           (progn (run-hooks 'bookmark-after-jump-hook) t)
           (if bookmark-automatically-show-annotations
               ;; if there is an annotation for this bookmark,
               ;; show it in a buffer.
               (bookmark-show-annotation bookmark))))))


(defun bookmark-set (&optional name parg)
  "Set a bookmark named NAME inside a file.
If name is nil, then the user will be prompted.
With prefix arg, will not overwrite a bookmark that has the same name
as NAME if such a bookmark already exists, but instead will \"push\"
the new bookmark onto the bookmark alist.  Thus the most recently set
bookmark with name NAME would be the one in effect at any given time,
but the others are still there, should you decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts the name of the last bookmark used in the buffer
\(as an aid in using a single bookmark name to track your progress
through a large file\).  If no bookmark was used, then C-u inserts the
name of the file being visited.

Use \\[bookmark-delete] to remove bookmarks \(you give it a name,
and it removes only the first instance of a bookmark with that name from
the list of bookmarks.\)"
  (interactive (list nil current-prefix-arg))
  (let* ((record (bookmark-make-record))
         (default (car record)))

    (bookmark-maybe-load-default-file)

    (setq bookmark-current-point (point))
    (setq bookmark-yank-point (point))
    (setq bookmark-current-buffer (current-buffer))

    (let ((str
           (or name
               (read-from-minibuffer
                (format "Set bookmark (%s): " default)
                nil
                bookmark-minibuffer-read-name-map
                nil nil default))))
      (and (string-equal str "") (setq str default))
      (bookmark-store str (cdr record) parg)

      ;; Ask for an annotation buffer for this bookmark
      (if bookmark-use-annotations
          (bookmark-edit-annotation str)
        (goto-char bookmark-current-point)))))

;;; File format stuff

;; The OLD format of the bookmark-alist was:
;;
;;       ((BOOKMARK-NAME . (FILENAME
;;                          STRING-IN-FRONT
;;                          STRING-BEHIND
;;                          POINT))
;;        ...)
;;
;; The NEW format of the bookmark-alist is:
;;
;;       ((BOOKMARK-NAME (filename   . FILENAME)
;;                       (front-context-string . STRING-IN-FRONT)
;;                       (rear-context-string  . STRING-BEHIND)
;;                       (position   . POINT)
;;                       (annotation . ANNOTATION)
;;                       (whatever   . VALUE)
;;                       ...
;;                       ))
;;        ...)
;;
;;
;; I switched to using an internal as well as external alist because I
;; felt that would be a more flexible framework in which to add
;; features.  It means that the order in which values appear doesn't
;; matter, and it means that arbitrary values can be added without
;; risk of interfering with existing ones.
;;
;; BOOKMARK-NAME is the string the user gives the bookmark and
;; accesses it by from then on.
;;
;; FILENAME is the location of the file in which the bookmark is set.
;;
;; STRING-IN-FRONT is a string of `bookmark-search-size' chars of
;; context in front of the point at which the bookmark is set.
;;
;; STRING-BEHIND is the same thing, but after the point.
;;
;; The context strings exist so that modifications to a file don't
;; necessarily cause a bookmark's position to be invalidated.
;; bookmark-jump will search for STRING-BEHIND and STRING-IN-FRONT in
;; case the file has changed since the bookmark was set.  It will
;; attempt to place the user before the changes, if there were any.
;; ANNOTATION is the annotation for the bookmark; it may not exist
;; (for backward compatibility), be nil (no annotation), or be a
;; string.

;; output of (funcall bookmark-make-record-function):

;; ((filename . "~/labo/bookmark-icicle-region-qp/bookmark+.el")
;;  (front-context-string . "))\n    ;; Set up")
;;  (rear-context-string . "record-function)")
;;  (position . 17799))

(defun bookmark-make-record-default (&optional point-only)
  "Return the record describing the location of a new bookmark.
Must be at the correct position in the buffer in which the bookmark is
being set.
If POINT-ONLY is non-nil, then only return the subset of the
record that pertains to the location within the buffer."
  `(,@(unless point-only `((filename . ,(bookmark-buffer-file-name))))
    (front-context-string
     . ,(if (>= (- (point-max) (point)) bookmark-search-size)
            (buffer-substring-no-properties
             (point)
             (+ (point) bookmark-search-size))
          nil))
    (rear-context-string
     . ,(if (>= (- (point) (point-min)) bookmark-search-size)
            (buffer-substring-no-properties
             (point)
             (- (point) bookmark-search-size))
          nil))
    (position . ,(point))))

;; When C-x r l
;; If fboundp ==> icicle-region
;;            ==> (region-active-p)
;; 1) use icicle-region to bookmark this file/region
;; 2) Notify the bookmark-alist to use icicle-region when jumping to bookmark. How? modify bookmark-make-record?

;; When jump to bookmark
;; 1) Modify jump to bookmark to use icicle-region if he find an icicle flag in bookmark alist.
;; 
;; So the problem is to set a nice entry in bookmark-alist with a flag:
;; (For this bookmark-name . use icicle-region)
;; Pass the arg bookmark-name==>alias icicle-region tag to icicle-region.


(defun bookmark-make-record ()
  "Return a new bookmark record (NAME . ALIST) for the current location."
  (let ((record (funcall bookmark-make-record-function)))
    ;; Set up default name.
    (if (stringp (car record))
        ;; The function already provided a default name.
        record
      (if (car record) (push nil record))
      (setcar record (or bookmark-current-bookmark (bookmark-buffer-name)))
      record)))

;; Not needed for Emacs 22+.
(unless (> emacs-major-version 21)
  (defun bookmark-menu-jump-other-window (event)
    "Jump to BOOKMARK (a point in some file) in another window.
See `bookmark-jump-other-window'."
    (interactive "e")
    (bookmark-popup-menu-and-apply-function
     'bookmark-jump-other-window "Jump to Bookmark (in another window)"
     event)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bookmark+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark+.el ends here
