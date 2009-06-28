;;; bookmark+.el --- Extensions to `bookmark.el'.
;;
;; Filename: bookmark+.el
;; Description: Extensions to `bookmark.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2009, Drew Adams, all rights reserved.
;; Created: Fri Sep 15 07:58:41 2000
;; Version: 21.0

;; URL: http://www.emacswiki.org/cgi-bin/wiki/bookmark+.el
;; Keywords: bookmarks, placeholders, annotations, search
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to `bookmark.el'.
;;
;;  Commands defined here:
;;
;;   `bookmark-toggle-use-only-regions', `bookmark+version-number'.
;;
;;  * User options defined here:
;;
;;    `bookmark-list-only-regions-flag',
;;    `bookmark-save-new-location-flag', `bookmark-use-region-flag'.
;;
;;  * Faces defined here:
;;
;;    `bookmark-directory', `bookmark-file', `bookmark-file-region',
;;    `bookmark-gnus', `bookmark-info-node',
;;    `bookmark-nonfile-buffer', `bookmark-remote-file',
;;    `bookmark-w3m-url'.
;;
;;  * Non-interactive functions defined here:
;;
;;    `bookmark-get-buffer-name', `bookmark-get-ecrs',
;;    `bookmark-get-ecs', `bookmark-get-end-position',
;;    `bookmark-get-fcrs', `bookmark-get-fcs', `bookmark-jump-gnus',
;;    `bookmark-jump-w3m', `bookmark-list-only-regions',
;;    `bookmark-make-gnus-record', `bookmark-make-record-function'
;;    (Emacs 20-22), `bookmark-make-record-region',
;;    `bookmark-make-w3m-record', `bookmark-menu-jump-other-window'
;;    (Emacs 20, 21), `bookmark-region-alist-only',
;;    `bookmark-region-handler', `bookmark+version'.
;;
;;  * Internal variables defined here:
;;
;;    `bookmark-region-search-size', `bookmark+-version-number'.
;;
;;
;;  ***** NOTE: The following functions defined in `bookmark.el'
;;              have been REDEFINED HERE:
;;
;;   `bookmark-bmenu-list', `bookmark-completing-read',
;;   `bookmark-default-handler', `bookmark-delete',
;;   `bookmark-get-bookmark' (Emacs 20-22),
;;   `bookmark-get-bookmark-record' (Emacs 20-22),
;;   `bookmark-get-handler' (Emacs 20-22), `bookmark-handle-bookmark'
;;   (Emacs 20-22),`bookmark-insert', `bookmark-insert-location',
;;   `bookmark-jump', `bookmark-jump-noselect',
;;   `bookmark-jump-other-window', `bookmark--jump-via' (Emacs 20-22),
;;   `bookmark-location', `bookmark-make-record' (Emacs 20-22),
;;   `bookmark-make-record-default', `bookmark-prop-get' (Emacs 20,
;;   21), `bookmark-prop-set' (Emacs 20, 21), `bookmark-relocate',
;;   `bookmark-rename', `bookmark-set', `bookmark-store'.
;;
;;
;;  ***** NOTE: The following functions defined in `info.el'
;;              have been REDEFINED HERE (Emacs 20-22):
;;
;;   `Info-bookmark-jump', `Info-bookmark-make-record'.
;;
;;
;;  * Features:
;;
;;    - You can bookmark a region of text, not just a position.
;;      When you jump to the bookmark, the region is activated, by
;;      default.
;;    - You can bookmark a buffer that is not associated with a file.
;;    - You can bookmark a region in a w3m buffer.
;;
;;  * Usage:
;;    Put this file in your `load-path'.
;;    Add to .emacs : (require 'bookmark+)
;;
;;    If you do not want bookmarked regions to be activated when you
;;    jump to them, customize option `bookmark-use-region-flag',
;;    setting it to nil.
;;
;;    You can temporarily flip the value of `bookmark-use-region-flag'
;;    by using a prefix arg (`C-u') when you jump to a bookmark.
;;
;;  Some of these functions bind `S-delete', to delete the current
;;  bookmark candidate during completion in Icicle mode (see Icicles:
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
;;     Only define bookmark-menu-jump-other-window if < Emacs 22.
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
(eval-when-compile (require 'cl))
(eval-when-compile (require 'gnus)) ; mail-header-id (really in `nnheader.el').

;; Quiet the byte-compiler
(defvar w3m-current-url)                ; Defined in `w3m.el'. @@@@@@@ Correct?
(when (< emacs-major-version 22) (defvar tramp-file-name-regexp)) ; Defined `tramp.el'.

(defconst bookmark+version-number "1.5.0")

(defun bookmark+version ()
  "Show version number of bookmark+.el"
  (interactive)
  (message "Bookmark+ Version %s" bookmark+version-number))

;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-key bookmark-map "o" 'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "q" 'bookmark-jump-other-window)
;;;###autoload
(define-key ctl-x-map "p" bookmark-map)
;;;###autoload
(define-key ctl-x-map "pj" 'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "T" 'bookmark-toggle-use-only-regions)


;;; User variables
(defcustom bookmark-use-region-flag t
  "*Non-nil means jumping to bookmark activates bookmarked region, if any."
  :type 'boolean :group 'bookmark)

(defcustom bookmark-list-only-regions-flag t
  "*Non-nil means bookmark commands use only bookmarks with regions."
  :type 'boolean :group 'bookmark)

(defcustom bookmark-region-search-size 40
  "The same as `bookmark-search-size' but specialized for bookmark regions."
  :type 'integer :group 'bookmark)

(defcustom bookmark-save-new-location-flag t
  "*Non-nil means save relocated bookmarks.
If nil, then the new bookmark location is visited, but it is not saved
as part of the bookmark definition."
  :type 'boolean :group 'bookmark)

(defcustom bookmark-retrieve-region-method-is 'lax
  "Method to use to retrieve region."
  :type 'symbol :group 'bookmark)

;;; Faces

(defface bookmark-nonfile-buffer
    '((t (:foreground "grey")))
  "*Face used for a bookmarked non-file buffer."
  :group 'bookmark)

(defface bookmark-file-region
    '((t (:foreground "Indianred2")))
  "*Face used for a bookmarked region in a local file."
  :group 'bookmark)

(defface bookmark-directory
    '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for a bookmarked local directory."
  :group 'bookmark)

(defface bookmark-file
    '((t (:foreground "Blue")))
  "*Face used for a bookmarked local file (without a region)."
  :group 'bookmark)

(defface bookmark-info-node
    '((t (:foreground "green")))
  "*Face used for a bookmarked Info node."
  :group 'bookmark)

(defface bookmark-w3m-url
    '((t (:foreground "yellow")))
  "*Face used for a bookmarked w3m url."
  :group 'bookmark)

(defface bookmark-gnus
    '((t (:foreground "magenta")))
  "*Face used for a gnus bookmark."
  :group 'bookmark)

(defface bookmark-remote-file
    '((t (:foreground "pink")))
  "*Face used for a bookmarked local file (without a region)."
  :group 'bookmark)


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
    (let* ((icicle-delete-candidate-object  'bookmark-delete) ; For `S-delete'.
           (completion-ignore-case          bookmark-completion-ignore-case)
           (default                         default)
           (prompt                          (if default
                                                (concat prompt (format " (%s): " default))
                                              (concat prompt ": ")))
           (str                             (completing-read prompt bookmark-alist nil 0 nil
                                                             'bookmark-history)))
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
;;;###autoload
(when (< emacs-major-version 23)

  ;; Same as vanilla Emacs 23+ definition.
  ;;
  (defun bookmark-get-bookmark (bookmark &optional noerror)
    "Return the bookmark record corresponding to BOOKMARK.
If BOOKMARK is already a bookmark record, just return it,
Otherwise look for the corresponding bookmark in `bookmark-alist'."
    (cond
      ((consp bookmark) bookmark)
      ((stringp bookmark)
       (or (if (fboundp 'assoc-string)  ; Emacs 22+.
               (assoc-string bookmark bookmark-alist bookmark-completion-ignore-case)
             (assoc bookmark bookmark-alist))
           (unless noerror (error "Invalid bookmark: `%s'" bookmark))))))

  ;; Same as vanilla Emacs 23+ definition.
  ;;
  (defun bookmark-get-bookmark-record (bookmark)
    "Return the guts of the entry for BOOKMARK in `bookmark-alist'.
That is, all information but the name."
    (let ((alist  (cdr (bookmark-get-bookmark bookmark))))
      ;; The bookmark objects can either look like (NAME ALIST) or
      ;; (NAME . ALIST), so we have to distinguish the two here.
      (if (and (null (cdr alist)) (consp (caar alist)))
          (car alist)
        alist))))


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
;; 1. Handles bookmarked regions and non-file buffer locations.
;; 2. Adds note about Icicles `S-delete' to doc string.
;;
;;;###autoload
(defun bookmark-jump (bookmark &optional use-region-p)
  "Jump to bookmark BOOKMARK (a point in some file).
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

If the file pointed to by BOOKMARK no longer exists, you are asked if
you wish to give the bookmark a new location.  If so, `bookmark-jump'
jumps to the new location and saves it.

If the bookmark represents a region, then it is selected if
`bookmark-use-region-flag' is not-nil or it is nil and you use a
prefix argument.  A prefix arg temporarily flips the value of
`bookmark-use-region-flag'.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Jump to bookmark"
                                               bookmark-current-bookmark)
                     current-prefix-arg))
  (unless bookmark (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark)
  (let ((bookmark-use-region-flag  (if use-region-p
                                       (not bookmark-use-region-flag)
                                     bookmark-use-region-flag)))
    (bookmark--jump-via bookmark 'switch-to-buffer)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Handles bookmarked regions and non-file buffer locations.
;;
;;;###autoload
(defun bookmark-jump-other-window (bookmark &optional use-region-p)
  "Jump to BOOKMARK (a point in some file) in another window.
See `bookmark-jump'."
  (interactive (list (bookmark-completing-read
                      "Jump to bookmark (in another window)"
                      bookmark-current-bookmark)
                     current-prefix-arg))
  (unless bookmark (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark)
  (let ((bookmark-use-region-flag  (if use-region-p
                                       (not bookmark-use-region-flag)
                                     bookmark-use-region-flag)))
    (bookmark--jump-via bookmark 'switch-to-buffer-other-window)))

;;;###autoload
(when (< emacs-major-version 23)

  ;; Same as vanilla Emacs 23+ definition.
  ;;
  (defun bookmark-prop-get (bookmark prop)
    "Return the property PROP of BOOKMARK, or nil if none."
    (cdr (assq prop (bookmark-get-bookmark-record bookmark))))

  (defun bookmark-prop-set (bookmark prop val)
    "Set the property PROP of BOOKMARK to VAL."
    (let ((cell  (assq prop (bookmark-get-bookmark-record bookmark))))
      (if cell
          (setcdr cell val)
        (nconc (bookmark-get-bookmark-record bookmark) (list (cons prop val))))))

  (defun bookmark-get-handler (bookmark)
    "Return the `handler' entry for BOOKMARK."
    (bookmark-prop-get bookmark 'handler))

  ;; Same as vanilla Emacs 23+ definition.
  ;;
  (defun bookmark--jump-via (bookmark display-function)
    (bookmark-handle-bookmark bookmark)
    (save-current-buffer (funcall display-function (current-buffer)))
    (let ((win  (get-buffer-window (current-buffer) 0)))
      (if win (set-window-point win (point))))
    ;; VANILLA EMACS FIXME: we used to only run bookmark-after-jump-hook in
    ;; `bookmark-jump' itself, but in none of the other commands.
    (run-hooks 'bookmark-after-jump-hook)
    (when bookmark-automatically-show-annotations (bookmark-show-annotation bookmark)))

  ;; Same as vanilla Emacs 23+ definition.
  ;;
  (defun bookmark-handle-bookmark (bookmark)
    "Call BOOKMARK's handler or `bookmark-default-handler' if it has none.
Changes current buffer and point and returns nil, or signals a `file-error'.
BOOKMARK can be a bookmark record used internally by some other
elisp package, or the name of a bookmark to be found in `bookmark-alist'."
    (condition-case err
        (funcall (or (bookmark-get-handler bookmark) 'bookmark-default-handler)
                 (bookmark-get-bookmark bookmark))
      (file-error
       ;; We were unable to find the marked file, so ask if user wants to
       ;; relocate the bookmark, else remind them to consider deletion.
       (when (stringp bookmark)
         ;; BOOKMARK can be either a bookmark name (found in `bookmark-alist') or a bookmark
         ;; object.  If an object, assume it's a bookmark used internally by some other
         ;; package.
         (let ((file  (bookmark-get-filename bookmark)))
           (when file                   ; Don't know how to relocate if file doesn't exist.
             (setq file  (expand-file-name file)) (ding)
             (cond ((y-or-n-p (concat (file-name-nondirectory file)
                                      " nonexistent.  Relocate \""
                                      bookmark "\"? "))
                    (bookmark-relocate bookmark) ; Try again
                    (funcall (or (bookmark-get-handler bookmark) 'bookmark-default-handler)
                             (bookmark-get-bookmark bookmark)))
                   (t
                    (message "Bookmark not relocated; consider removing it \(%s\)." bookmark)
                    (signal (car err) (cdr err)))))))))
    (when (stringp bookmark) (setq bookmark-current-bookmark bookmark))
    nil)

  ;; Same as vanilla Emacs 23+ definition.
  ;;
  (defun bookmark-jump-noselect (bookmark)
    "Return the location pointed to by the bookmark BOOKMARK.
The return value has the form (BUFFER . POINT)."
    (save-excursion
      (bookmark-handle-bookmark bookmark)
      (cons (current-buffer) (point)))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Handles region bookmarks and buffer (non-file) bookmarks.
;;
;;;###autoload
(defun bookmark-bmenu-list ()
  "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
  (interactive)
  (bookmark-maybe-load-default-file)
  (if (interactive-p)
      (switch-to-buffer (get-buffer-create "*Bookmark List*"))
    (set-buffer (get-buffer-create "*Bookmark List*")))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "% Bookmark+\n- --------\n")
    (add-text-properties (point-min) (point)
                         '(font-lock-face bookmark-menu-heading))
    (mapc
     (lambda (full-record)
       ;; If a bookmark has an annotation, prepend a "*" in the list of bookmarks.
       (let ((annotation  (bookmark-get-annotation (bookmark-name-from-full-record full-record))))
         (insert (if (and annotation (not (string-equal annotation "")))  " *"  "  "))
         (let* ((start         (point))
                (name          (bookmark-name-from-full-record full-record))
                (isfile        (bookmark-get-filename name))
                (istramp       (and isfile (boundp 'tramp-file-name-regexp)
                                    (save-match-data (string-match tramp-file-name-regexp isfile))))
                (isregion      (and (bookmark-get-end-position name)
                                (/= (bookmark-get-position name)
                                    (bookmark-get-end-position name))))
                (isannotation  (bookmark-get-annotation name))
                (ishandler     (bookmark-get-handler name))
                (isgnus        (assq 'group full-record))
                (isbuf         (bookmark-get-buffer-name name)))
	   (insert name)
	   (add-text-properties
            start  (save-excursion (re-search-backward "[^ \t]") (1+ (point)))
            ;; @@@@@@@ FIXME LATER
            ;;         Aren't some of these conditions redundant?
            ;;         My impression is you could start with the ISTRAMP and Info clauses,
            ;;         then maybe a NOT ISFILE clause, and such reordering would simplify
            ;;         the conditions used (and so help readability).
            (cond ((and isfile (not istramp) (file-directory-p isfile)) ; Local directory
                   '(mouse-face highlight
                     follow-link t face 'bookmark-directory
                     help-echo "mouse-2: go to this dired buffer in other window"))
                  ((and isfile (not istramp) (not (file-directory-p isfile))
                        (file-exists-p isfile) isregion) ; Local file with region
                   '(mouse-face highlight follow-link t
                     face 'bookmark-file-region
                     help-echo "mouse-2: go to this file with region"))
                  ((and isfile (not istramp) (not (file-directory-p isfile))
                        (file-exists-p isfile)) ; Local file without region
                   '(mouse-face highlight follow-link t face 'bookmark-file
                     help-echo "mouse-2: go to this file in other window"))
                  (isgnus ; Gnus
                   '(mouse-face highlight
                     follow-link t face 'bookmark-gnus
                     help-echo "mouse-2: Go to this gnus buffer"))
                  ((and isbuf (not isfile)) ; Buffer without a file
                   '(mouse-face highlight follow-link t face 'bookmark-nonfile-buffer 
                     help-echo "mouse-2: go to this non--buffer-filename"))
                  ((and (string= isbuf "*w3m*") isfile (not (file-exists-p isfile))) ; w3m url
                   '(mouse-face highlight follow-link t face 'bookmark-w3m-url
                     help-echo "mouse-2: go to this w3m url"))
                  ((or (eq ishandler 'Info-bookmark-jump) ; Info buffer
                       (and (string= isbuf "*info*") (and isfile (not (file-exists-p isfile)))))
                   '(mouse-face highlight follow-link t face 'bookmark-info-node
                     help-echo "mouse-2: go to this info buffer"))
                  (istramp              ; Remote file
                   '(mouse-face highlight follow-link t face 'bookmark-remote-file
                     help-echo "mouse-2: go to this tramp buffer"))))
           (insert "\n"))))
     (bookmark-maybe-sort-alist)))
  (goto-char (point-min))
  (forward-line 2)
  (bookmark-bmenu-mode)
  (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames t)))

(defun bookmark-get-buffer-name (bookmark)
  "Return the buffer-name of BOOKMARK."
  (bookmark-prop-get bookmark 'buffer-name))

(defun bookmark-get-end-position (bookmark)
  "Return the end-position of REGION in BOOKMARK."
  (bookmark-prop-get bookmark 'end-position))

(defun bookmark-region-alist-only ()
  "Create an alist with only bookmarks with region."
  (loop for i in bookmark-alist
     for b = (and (bookmark-get-end-position i)
                  (/= (bookmark-get-position i)
                      (bookmark-get-end-position i)))
     if b
     collect i))

(defun bookmark-list-only-regions ()
  "Return only the elements of `bookmark-alist' that have a recorded region."
  (let ((bookmark-alist (bookmark-region-alist-only)))
    (call-interactively #'bookmark-bmenu-list)))

;; (find-fline "~/download/bookmark+-2009-06-13a-DREW.el" "defun bookmark-toggle-use-only-regions")
;;;###autoload
(defun bookmark-toggle-use-only-regions ()
  "Toggle `bookmark-list-only-regions-flag', and redisplay bookmark list."
  (interactive)
  (setq bookmark-list-only-regions-flag (not bookmark-list-only-regions-flag))
  (if bookmark-list-only-regions-flag
      (bookmark-list-only-regions)
      (call-interactively #'bookmark-bmenu-list)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Location returned can be a buffer name, instead of a file name.
;;
(defun bookmark-location (bookmark)
  "Return the name of the file or buffer associated with BOOKMARK."
  (bookmark-maybe-load-default-file)
  (or (bookmark-get-filename bookmark)
      (bookmark-get-buffer-name bookmark)
      (error "Bookmark has no file or buffer name: %S" bookmark)))

;; (find-fline "/usr/share/emacs/23.0.95/lisp/bookmark.el" "defun bookmark-set ")
;; (find-fline "/usr/share/emacs/23.0.95/lisp/bookmark.el" "defun bookmark-make-record")
;; (find-fline "/usr/share/emacs/23.0.95/lisp/bookmark.el" "defun bookmark-make-record-default")
;; (find-fline "/usr/share/emacs/23.0.95/lisp/info.el" "defun Info-bookmark-make-record")

(defun bookmark-get-fcs (breg ereg regionp)
  "Create the `bookmark-alist' entry `front-context-string'.
The meaning depends on whether the buffer to bookmark has an active
region.

If so, return the string from the region beginning to
`bookmark-region-search-size' characters after that position.

If not, return the string between point and
`bookmark-region-search-size' chars before point."
  ;; @@@ Is that sentence correct? You had said "`bookmark-region-search-size'
  ;;     BEFORE REGION", but there isn't necessarily any region in this case.
  (if regionp
      (buffer-substring-no-properties
       breg
       (+ breg (min bookmark-region-search-size (- ereg breg))))
    (if (>= (- (point-max) (point)) bookmark-search-size)
        (buffer-substring-no-properties
         (point)
         (+ (point) bookmark-search-size))
      nil)))

(defun bookmark-get-ecs (breg ereg regionp)
  "Create the `bookmark-alist' entry `rear-context-string'.
The meaning depends on whether the buffer to bookmark has an active
region.

If so, return the string from `bookmark-region-search-size' characters
before the region beginning to the end of the region.

If not, return the string between point and
`bookmark-region-search-size' chars before point."
  ;; @@@ Is that sentence correct? You had said "`bookmark-region-search-size'
  ;;     BEFORE REGION", but there isn't necessarily any region in this case.
  (if regionp
      (buffer-substring-no-properties
       ereg (- ereg (min bookmark-region-search-size (- ereg breg))))
    (if (>= (- (point) (point-min)) bookmark-search-size)
        (buffer-substring-no-properties (point) (- (point) bookmark-search-size))
      nil)))

(defun bookmark-get-fcrs (breg regionp)
  "Create the `bookmark-alist' entry `front-context-region-string'.
This string is just before the region beginning."
  (if (not regionp)
      nil
    (goto-char breg)
    (re-search-backward ".[^ ]" nil t)
    (buffer-substring-no-properties (max (- (point) bookmark-region-search-size) (point-min))
                                    breg)))

(defun bookmark-get-ecrs (ereg regionp)
  "Create the `bookmark-alist' entry `rear-context-region-string'.
This string is just after the region end."
  (if (not regionp)
      nil
    (goto-char ereg)
    (re-search-forward "^.*[^ \n]" nil t)
    (beginning-of-line)
    (buffer-substring-no-properties ereg (+ (point) (min bookmark-region-search-size
                                                         (- (point-max) (point)))))))

(defun bookmark-retrieve-region-lax (forward-str behind-str str-bef str-aft pos end-pos)
  ;; Relocate region if it has moved.
  (unless (and (string= forward-str (buffer-substring-no-properties
                                     (point) (+ (point) (length forward-str))))
               (save-excursion
                 (goto-char end-pos)
                 (string= behind-str (buffer-substring-no-properties
                                      (point) (- (point) (length forward-str))))))
    (goto-char (point-min))    ; Start at bob and search forward.
    (let (beg end)
      (if (search-forward behind-str (point-max) t) ; Find END, using `behind-str'.
          (setq end  (point))
          (when (search-forward str-aft (point-max) t) ; Find END, using `str-aft'.
            (setq end  (match-beginning 0))
            (when end
              (goto-char end)
              ;; If `str-aft' moved, then look for END one or more lines back.
              (while (and (not (bobp))
                          (not (save-excursion ; This is `looking-back', for older Emacs.
                                 (and (re-search-backward "\\(.[^ \n]\\)\\=" nil t)
                                      (point)))))
                (forward-char -1))
              (setq end  (point)))))
      ;; If failed to find END, go to eob and search backward from there.
      (unless end (goto-char (point-max)))
      (if (search-backward forward-str (point-min) t) ; Find BEG, using `forward-str'.
          (setq beg  (point))
          (when (search-backward str-bef (point-min) t) ; Find BEG, using `str-bef'.
            (setq beg (match-end 0))
            (when beg
              (goto-char beg)
              ;; If `str-bef' moved, then look for BEG one or more lines forward.
              (while (and (not (eobp)) (not (looking-at ".[^ \n]"))) (forward-char 1))
              (setq beg (point)))))

      ;; @@@ FIXME: Should we save new context string if only one position was relocated?

      ;; Save new location to `bookmark-alist' only if BEG or END was found.
      ;; If only one of them was found, the located region is only approximate.
      ;; If both were found, it is exact.
      (cond ((and beg end) (setq pos      beg
                                 end-pos  end))
            ((and beg (not end)) (setq pos  beg))
            ((and (not beg) end) (setq end-pos  end))
            (t (setq region-retrieved-p  nil)))
      (when (and region-retrieved-p bookmark-save-new-location-flag)
        (bookmark-prop-set bmk 'front-context-string (bookmark-get-fcs pos end-pos t))
        (bookmark-prop-set bmk 'rear-context-string (bookmark-get-ecs pos end-pos t))
        (bookmark-prop-set bmk 'front-context-region-string (bookmark-get-fcrs pos t))
        (bookmark-prop-set bmk 'rear-context-region-string (bookmark-get-ecrs end-pos t))
        (bookmark-prop-set bmk 'position pos)
        (bookmark-prop-set bmk 'end-position end-pos))))

  (cond (region-retrieved-p
         (goto-char pos)
         (push-mark end-pos 'nomsg 'activate)
         (setq deactivate-mark  nil)
         (message "Region is from %d to %d" pos end-pos))
        (t
         ;; Region doesn't exist anymore.  Go to old start position.  Don't push-mark.
         (goto-char pos) (beginning-of-line)
         (message "No region from %d to %d" pos end-pos))))


;;;###autoload
(when (< emacs-major-version 23)

  ;; REPLACES ORIGINAL in `bookmark.el'.
  ;;
  ;; Uses `bookmark-make-record'.
  ;;
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
    (let* ((record   (bookmark-make-record))
           (default  (car record)))
      (bookmark-maybe-load-default-file)
      (setq bookmark-current-point   (point)
            bookmark-yank-point      (point)
            bookmark-current-buffer  (current-buffer))
      (let ((str         (or name (read-from-minibuffer
                                   (format "Set bookmark (%s): " default) nil
                                   (let ((map  (copy-keymap minibuffer-local-map)))
                                     (define-key map "\C-w" 'bookmark-yank-word)
                                     (define-key map "\C-u" 'bookmark-insert-current-bookmark)
                                     map)
                                   nil nil default)))
            (annotation  nil))
        (and (string-equal str "") (setq str  default))
        (bookmark-store str (cdr record) parg)
        ;; Ask for an annotation buffer for this bookmark
        (if bookmark-use-annotations
            (bookmark-edit-annotation str)
          (goto-char bookmark-current-point)))))

  ;; Same as vanilla Emacs 23+ definition.
  ;;
  (defun bookmark-store (name alist no-overwrite)
    "Store the bookmark NAME with data ALIST.
If NO-OVERWRITE is non-nil and another bookmark of the same name already
exists in `bookmark-alist', record the new bookmark without throwing away the
old one."
    (bookmark-maybe-load-default-file)
    (let ((stripped-name  (copy-sequence name)))
      (or (featurep 'xemacs)
          ;; XEmacs's `set-text-properties' doesn't work on free-standing strings.
          (set-text-properties 0 (length stripped-name) nil stripped-name))
      (if (and (not no-overwrite) (bookmark-get-bookmark stripped-name 'no-error))
          ;; Existing bookmark under that name and no prefix arg means just overwrite old.
          ;; Use the new (NAME . ALIST) format.
          (setcdr (bookmark-get-bookmark stripped-name) alist)
        (push (cons stripped-name alist) bookmark-alist))
      (setq bookmark-current-bookmark          stripped-name
            bookmark-alist-modification-count  (1+ bookmark-alist-modification-count))
      (when (bookmark-time-to-save-p) (bookmark-save))
      (setq bookmark-current-bookmark  stripped-name)
      (bookmark-bmenu-surreptitiously-rebuild-list)))

  ;; Same as vanilla Emacs 23+ definition.
  ;;
  (defvar bookmark-make-record-function 'bookmark-make-record-default
    "A function that should be called to create a bookmark record.
Modes may set this variable buffer-locally to enable bookmarking of
locations that should be treated specially, such as Info nodes,
news posts, images, pdf documents, etc.

The function will be called with no arguments.
It should signal a user error if it is unable to construct a record for
the current location.

The returned record should be a cons cell of the form (NAME . ALIST)
where ALIST is as described in `bookmark-alist' and may typically contain
a special cons (handler . SOME-FUNCTION) which sets the handler function
that should be used to open this bookmark instead of
`bookmark-default-handler'.  The handler should follow the same calling
convention as the one used by `bookmark-default-handler'.

NAME is a suggested name for the constructed bookmark.  It can be nil
in which case a default heuristic will be used.  The function can also
equivalently just return ALIST without NAME.")

  ;; Same as vanilla Emacs 23+ definition.
  ;;
  (defun bookmark-make-record ()
    "Return a new bookmark record (NAME . ALIST) for the current location."
    (let ((record  (funcall bookmark-make-record-function)))
      ;; Set up default name.
      (if (stringp (car record))
          record                        ; The function already provided a default name.
        (when (car record) (push nil record))
        (setcar record  (or bookmark-current-bookmark (bookmark-buffer-name)))
        record))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Handles regions and non-file buffers.
;;
(defun bookmark-make-record-default (&optional point-only)
  "Return the record describing the location of a new bookmark.
Must be at the correct position in the buffer in which the bookmark is
being set.
If POINT-ONLY is non-nil, then only return the subset of the
record that pertains to the location within the buffer."
  (let* ((isregion (and transient-mark-mode
                        mark-active
                        (not (eq (mark) (point)))))
         (isdired (car (rassq (current-buffer) dired-buffers)))
         (beg (if isregion
                  (region-beginning)
                  (point)))
         (end (if isregion
                  (region-end)
                  (point)))
         (buf (buffer-name))
         (fcs (bookmark-get-fcs beg end isregion))
         (ecs (bookmark-get-ecs beg end isregion))
         (fcrs (bookmark-get-fcrs beg isregion))
         (ecrs (bookmark-get-ecrs end isregion)))
    `(,@(unless point-only `((filename . ,(cond ((buffer-file-name (current-buffer))
                                                 (bookmark-buffer-file-name))
                                                (isdired)
                                                (t
                                                 nil)))))
        (buffer-name . ,buf)
        (front-context-string . ,fcs)
        (rear-context-string . ,ecs)
        (front-context-region-string . ,fcrs)
        (rear-context-region-string . ,ecrs)
        (position . ,beg)
        (end-position . ,end))))

;; @@@@@@ FIXME LATER
;;        Don't add stuff that has a nil value to the bookmark record (it is not used).
;;        For example, don't add the buffer name if the file name is present.
;;        This is not critical and can be done later.


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Support regions and buffer names.
;;
(defun bookmark-default-handler (bmk)
  "Default handler to jump to a bookmark location.
BMK is a bookmark record.  Return nil or signal a `file-error.
Changes current buffer and point."
  (let* ((file                   (bookmark-get-filename bmk))
         (buf                    (bookmark-prop-get bmk 'buffer-name))
         (forward-str            (bookmark-get-front-context-string bmk))
         (behind-str             (bookmark-get-rear-context-string bmk))
         (str-bef                (bookmark-prop-get bmk 'front-context-region-string))
         (str-aft                (bookmark-prop-get bmk 'rear-context-region-string))
         (pos                    (bookmark-get-position bmk))
         (end-pos                (bookmark-prop-get bmk 'end-position))
         (region-retrieved-p     t))

    ;; Bookmark with a region.  Go to it and select region.
    (cond ((and bookmark-use-region-flag end-pos (/= pos end-pos))
           ;; Get buffer.
           (if (and file (file-readable-p file) (not (buffer-live-p buf)))
               (with-current-buffer (find-file-noselect file) (setq buf  (buffer-name)))
             ;; No file found.  If no buffer either, then signal that file doesn't exist.
             (unless (and buf (get-buffer buf))
               (signal 'file-error `("Jumping to bookmark" "No such file or directory"
                                     (bookmark-get-filename bmk)))))
           (pop-to-buffer buf)
           (raise-frame)
           (if (<= pos (point-max))
               (goto-char pos)
             (goto-char (point-max))
             (error "Bookmark position is beyond buffer end"))
           (if (eq bookmark-retrieve-region-method-is 'lax)
               (bookmark-retrieve-region-lax forward-str behind-str str-bef str-aft pos end-pos)))

          ;; Single-position bookmark (no region).  Go to it.
          (t
           (if (and file (file-readable-p file) (not (buffer-live-p buf)))
               (with-current-buffer (find-file-noselect file) (setq buf  (buffer-name)))
             ;; No file found we search for a buffer non--filename
             ;; if not found signal file doesn't exist anymore
             (unless (and buf (get-buffer buf))
               (signal 'file-error `("Jumping to bookmark" "No such file or directory"
                                     (bookmark-get-filename bmk)))))
           (pop-to-buffer buf)
           (setq deactivate-mark  t)
           (raise-frame)
           (goto-char pos)
           ;; Search forward first.  Then, if FORWARD-STR exists and
           ;; was found in the file, we can search backward for BEHIND-STR.
           ;; Rationale is that if text was inserted between the two in the
           ;; file, it's better to be put point before it so you can read it,
           ;; rather than after and remain perhaps unaware of the changes.
           (when (and forward-str (search-forward forward-str (point-max) t))
             (goto-char (match-beginning 0)))
           (when (and behind-str (search-backward behind-str (point-min) t))
             (goto-char (match-end 0)))
           nil))))

;;;###autoload
(when (< emacs-major-version 23)
  (defun Info-bookmark-make-record ()
    `(,Info-current-node
      ,@(bookmark-make-record-default 'point-only)
      (filename . ,Info-current-file)
      (info-node . ,Info-current-node)
      (handler . Info-bookmark-jump)))

  (defun Info-bookmark-jump (bmk)
    ;; This implements the `handler' function interface for record type returned
    ;; by `Info-bookmark-make-record', which see.
    (let* ((file                   (bookmark-prop-get bmk 'filename))
           (info-node              (bookmark-prop-get bmk 'info-node))
           (buf (save-window-excursion  ;FIXME: doesn't work with frames!
                  (Info-find-node file info-node) (current-buffer))))
      ;; Use bookmark-default-handler to move to the appropriate location
      ;; within the node.
      (bookmark-default-handler
       (list* "" `(buffer . ,buf) (bookmark-get-bookmark-record bmk)))))

  (add-hook 'Info-mode-hook (lambda ()
                              (set (make-local-variable 'bookmark-make-record-function)
                                   'Info-bookmark-make-record))))

;; W3M support
(defun bookmark-make-w3m-record ()
  "Make a special entry for w3m buffers."
  (require 'w3m)                        ; For `w3m-current-url'.
  `(,@(bookmark-make-record-default 'point-only)
    (filename . ,w3m-current-url)
    (handler . bookmark-jump-w3m)))

(add-hook 'w3m-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'bookmark-make-w3m-record)))

(defun bookmark-jump-w3m (bmk)
  ;; Handler function for record returned by `bookmark-make-w3m-record'.
  (let* ((file  (bookmark-prop-get bmk 'filename))
         (buf   (bookmark-prop-get bmk 'buffer)))
    (w3m-browse-url file)
    (with-current-buffer "*w3m*" (while (eq (point-min) (point-max)) (sit-for 1)))
    (bookmark-default-handler (list* "" `(buffer . ,buf) (bookmark-get-bookmark-record bmk)))))

;; GNUS support.  Does not handle regions.
(defun bookmark-make-gnus-record ()
  "Make a bookmark entry for gnus buffers."  
  (require 'gnus)
  (unless (and (eq major-mode 'gnus-summary-mode) gnus-article-current)
    (error "Please retry from the Gnus summary buffer")) ;[1]
  (let* ((grp   (car gnus-article-current))
         (art   (cdr gnus-article-current))
         (head  (gnus-summary-article-header art))
         (id    (mail-header-id head)))
    `(,@(bookmark-make-record-default 'point-only)
      (group . ,grp)
      (article . ,art)
      (message-id . ,id)
      (handler . bookmark-jump-gnus))))

(add-hook 'gnus-summary-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'bookmark-make-gnus-record)))

;; Raise an error if we try to bookmark from here [1]
(add-hook 'gnus-article-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'bookmark-make-gnus-record)))

(defun bookmark-jump-gnus (bmk)
  ;; Handler' function for record returned by `bookmark-make-gnus-record'.
  (let ((group    (bookmark-prop-get bmk 'group))
        (article  (bookmark-prop-get bmk 'article))
        (id       (bookmark-prop-get bmk 'message-id))
        (buf      (bookmark-prop-get bmk 'buffer)))
    (gnus-fetch-group group (list article))
    (gnus-summary-insert-cached-articles)
    (gnus-summary-goto-article id nil 'force)
    (bookmark-default-handler (list* "" `(buffer . ,buf) (bookmark-get-bookmark-record bmk)))))

;; Not needed for Emacs 22+.
;;;###autoload
(when (< emacs-major-version 22)
  (defun bookmark-menu-jump-other-window (event)
    "Jump to BOOKMARK (a point in some file) in another window.
See `bookmark-jump-other-window'."
    (interactive "e")
    (bookmark-popup-menu-and-apply-function 'bookmark-jump-other-window
                                            "Jump to Bookmark (in another window)" event)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bookmark+-2009-06-27d-DREW)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark+.el ends here
