;;; bookmark+.el --- Extensions to `bookmark.el'.

;; Filename: bookmark+.el
;; Description: Extensions to `bookmark.el'.

;; Author: Drew Adams
;;         Thierry Volpiatto
;; Maintainer: Drew Adams
;;             Thierry Volpiatto
;; Copyright (C) 2000-2009, Drew Adams, all rights reserved.
;; Copyright (C) 2009, Thierry Volpiatto, all rights reserved.
;; Created: Fri Sep 15 07:58:41 2000

;; URL: http://www.emacswiki.org/cgi-bin/wiki/bookmark+.el
;; X-URL: http://mercurial.intuxication.org/hg/bookmark-icicle-region/

;; Keywords: bookmarks, placeholders, annotations, search
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x,
;; GNU Emacs 23.x


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

;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to `bookmark.el'.

;;  Commands defined here:
;;
;;   `bookmark-bmenu-list-only-regions',
;;   `bookmark-bmenu-list-only-gnus-entries',
;;   `bookmark-bmenu-list-only-w3m-entries',
;;   `bookmark-bmenu-list-only-info-entries',
;;   `bookmark-bmenu-list-only-files-entries',
;;   `bookmark+version-number'.

;;  * User options defined here:
;;
;;    `bookmark-relocate-region-function',
;;    `bookmark-save-new-location-flag', `bookmark-use-region-flag'.

;;  * Faces defined here:
;;
;;    `bookmark-directory', `bookmark-file', `bookmark-file-region',
;;    `bookmark-gnus', `bookmark-info-node',
;;    `bookmark-nonfile-buffer', `bookmark-remote-file',
;;    `bookmark-w3m-url', `bookmark-su-or-sudo' .

;;  * Non-interactive functions defined here:
;;
;;    `bookmark-get-buffer-name', `bookmark-get-ecrs',
;;    `bookmark-get-end-position', `bookmark-get-fcrs',
;;    `bookmark-goto-position', `bookmark-jump-gnus',
;;    `bookmark-jump-w3m', `bookmark-list-only-regions',
;;    `bookmark-make-gnus-record', `bookmark-make-record-function',
;;    (Emacs 20-22), `bookmark-make-record-region',
;;    `bookmark-make-w3m-record', `bookmark-menu-jump-other-window',
;;    `bookmark-position-after-whitespace',
;;    `bookmark-position-before-whitespace', (Emacs 20, 21),
;;    `bookmark-record-end-context-region-string',
;;    `bookmark-record-front-context-region-string',
;;    `bookmark-record-front-context-string',
;;    `bookmark-record-rear-context-string',
;;    `bookmark-region-alist-only', `bookmark-region-handler',
;;    `bookmark-relocate-region-lax',
;;    `bookmark-relocate-region-strict',
;;    `bookmark-save-new-region-location', `bookmark+version'.

;;  * Internal variables defined here:
;;
;;    `bookmark-region-search-size', `bookmark+-version-number'.
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
;; See Change log at:
;; http://mercurial.intuxication.org/hg/bookmark-icicle-region/
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

;;; Code:

(require 'bookmark)
(eval-when-compile (require 'cl))
(eval-when-compile (require 'gnus)) ; mail-header-id (really in `nnheader.el')

;; Quiet the byte-compiler
(defvar w3m-current-url)                ; Defined in `w3m.el'.
(defvar gnus-article-current)           ; Defined in `gnus-sum.el'.
(defvar tramp-file-name-regexp)         ; Defined in `tramp.el'.
(defvar bookmark-make-record-function)  ; Defined in `bookmark.el'.

(defconst bookmark+version-number "2.0.7")

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
(define-key bookmark-map "R" 'bookmark-bmenu-list-only-regions)
;;;###autoload
(define-key bookmark-map "G" 'bookmark-bmenu-list-only-gnus-entries)
;;;###autoload
(define-key bookmark-map "W" 'bookmark-bmenu-list-only-w3m-entries)
;;;###autoload
(define-key bookmark-map "A" 'bookmark-bmenu-list)
;;;###autoload
(define-key bookmark-map "F" 'bookmark-bmenu-list-only-files-entries)
;;;###autoload
(define-key bookmark-map "I" 'bookmark-bmenu-list-only-info-entries)

(defun bookmark-bmenu-list-set-extra-keys ()
  "Define extras keys in the `bookmark-bmenu-mode-map' space."
  (define-key bookmark-bmenu-mode-map "W" 'bookmark-bmenu-list-only-w3m-entries)
  (define-key bookmark-bmenu-mode-map "I" 'bookmark-bmenu-list-only-info-entries)
  (define-key bookmark-bmenu-mode-map "G" 'bookmark-bmenu-list-only-gnus-entries)
  (define-key bookmark-bmenu-mode-map "F" 'bookmark-bmenu-list-only-files-entries)
  (define-key bookmark-bmenu-mode-map "R" 'bookmark-bmenu-list-only-regions))
(add-hook 'bookmark-bmenu-mode-hook 'bookmark-bmenu-list-set-extra-keys) 


;;; User variables
(defcustom bookmark-use-region-flag t
  "*Non-nil means jumping to bookmark activates bookmarked region, if any."
  :type 'boolean :group 'bookmark)

(defcustom bookmark-region-search-size 40
  "The same as `bookmark-search-size' but specialized for bookmark regions."
  :type 'integer :group 'bookmark)

(defcustom bookmark-save-new-location-flag t
  "*Non-nil means save relocated bookmarks.
If nil, then the new bookmark location is visited, but it is not saved
as part of the bookmark definition."
  :type 'boolean :group 'bookmark)

(defcustom bookmark-relocate-region-function
  'bookmark-relocate-region-default
  "Default function to relocate bookmarked region."
  :type 'function :group 'bookmark)

(defcustom bookmark-su-or-sudo-regexp
  "\\(/su:\\|/sudo:\\)"
  "Regexp to recognize su or sudo Tramp bookmarks."
  :type 'regexp :group 'bookmark)

(defcustom bookmark-w3m-allow-multi-tabs t
  "*Non-nil means jump to w3m bookmark in a new session."
  :type 'boolean :group 'bookmark)

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
  "*Face used for a bookmarked tramp remote file (/ssh:)."
  :group 'bookmark)

(defface bookmark-su-or-sudo
    '((t (:foreground "red")))
  "*Face used for a bookmarked tramp file (/su: or /sudo:)."
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
(defun bookmark-bmenu-list (&optional title)
  "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
  (interactive)
  (bookmark-maybe-load-default-file)
  (if (interactive-p)
      (switch-to-buffer (get-buffer-create "*Bookmark List*"))
    (set-buffer (get-buffer-create "*Bookmark List*")))
  (let* ((inhibit-read-only t)
         (alternate-title   (if title title "% Bookmark+"))
         (len-alt-title     (- (length alternate-title) 2)))
    (erase-buffer)
    (insert (format "%s\n- %s\n" alternate-title (make-string len-alt-title ?-)))
    (add-text-properties (point-min) (point)
                         '(font-lock-face bookmark-menu-heading))
    (mapc
     (lambda (full-record)
       ;; If a bookmark has an annotation, prepend a "*" in the list of bookmarks.
       (let ((annotation  (bookmark-get-annotation
                           (bookmark-name-from-full-record full-record))))
         (insert (if (and annotation (not (string-equal annotation "")))  " *"  "  "))
         (let* ((start         (point))
                (name          (bookmark-name-from-full-record full-record))
                (isfile        (bookmark-get-filename name))
                (istramp       (and isfile (boundp 'tramp-file-name-regexp)
                                    (save-match-data
                                      (string-match tramp-file-name-regexp isfile))))
                (isssh         (and istramp
                                    (string-match "/ssh:" isfile)))
                (issu          (and istramp
                                    (string-match bookmark-su-or-sudo-regexp isfile)))
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
            (cond ((or (eq ishandler 'Info-bookmark-jump) (string= isbuf "*info*")) ; Info
                   '(mouse-face highlight follow-link t face 'bookmark-info-node
                     help-echo "mouse-2: Go to this Info buffer"))
                  (isgnus               ; Gnus
                   '(mouse-face highlight follow-link t face 'bookmark-gnus
                     help-echo "mouse-2: Go to this Gnus buffer"))
                  ((and (string= isbuf "*w3m*") isfile (not (file-exists-p isfile))) ; W3m URL
                   `(mouse-face highlight follow-link t face 'bookmark-w3m-url
                     help-echo (format "mouse-2 Goto URL: %s",isfile)))
                  (isssh              ; Remote file
                   `(mouse-face highlight follow-link t face 'bookmark-remote-file
                     help-echo (format "mouse-2 Goto remote file: %s",isfile)))
                  ((and issu (not (root-or-sudo-logged-p))) ; Root or sudo bookmarks
                   `(mouse-face highlight follow-link t face 'bookmark-su-or-sudo
                     help-echo (format "mouse-2 Goto file: %s",isfile)))
                  ((and isfile (file-directory-p isfile)) ; Local directory
                   `(mouse-face highlight follow-link t face 'bookmark-directory
                     help-echo (format "mouse-2 Goto dired: %s",isfile)))
                  ((and isfile (file-exists-p isfile) isregion) ; Local file with region
                   `(mouse-face highlight follow-link t face 'bookmark-file-region
                     help-echo (format "mouse-2 Find region in file: %s",isfile)))
                  ((and isfile (file-exists-p isfile)) ; Local file without region
                   `(mouse-face highlight follow-link t face 'bookmark-file
                     help-echo (format "mouse-2 Goto file: %s",isfile)))
                  ((and isbuf (not isfile)) ; Buffer without a file
                   `(mouse-face highlight follow-link t face 'bookmark-nonfile-buffer 
                     help-echo (format "mouse-2 Goto buffer: %s",isbuf)))))
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

(defun root-or-sudo-logged-p ()
  "Return t if User is logged as root or sudo with Tramp.
Otherwise nil."
  (let ((su-or-sudo-regex "\\(su\\|sudo\\)"))
    (catch 'break
      (dolist (i (mapcar #'buffer-name (buffer-list)))
        (when (string-match (format "*tramp/%s ." su-or-sudo-regex) i)
          (throw 'break t))))))

;;; Filter functions
(defun bookmark-region-alist-only ()
  "Create an alist with only bookmarks with region."
  (loop for i in bookmark-alist
     for b = (and (bookmark-get-end-position i)
                  (/= (bookmark-get-position i)
                      (bookmark-get-end-position i)))
     if b collect i))

(defun bookmark-gnus-alist-only ()
  "Return an alist with only gnus entries."
  (loop for i in bookmark-alist
     if (eq (bookmark-get-handler i) 'bookmark-jump-gnus)
     collect i))  

(defun bookmark-w3m-alist-only ()
  "Return an alist with only w3m entries."
  (loop for i in bookmark-alist
     if (eq (bookmark-get-handler i) 'bookmark-jump-w3m)
     collect i))  

(defun bookmark-info-alist-only ()
  "Return an alist with only info entries."
  (loop for i in bookmark-alist
     if (eq (bookmark-get-handler i) 'Info-bookmark-jump)
     collect i))  

(defun bookmark-remote-alist-only ()
  "Return an alist with only tramp entries."
  (loop for i in bookmark-alist
     for a = (bookmark-get-filename i)
     for b = (and a (boundp 'tramp-file-name-regexp)
                  (save-match-data
                    (string-match tramp-file-name-regexp a)))
     if b collect i))

(defun bookmark-vanilla-alist-only (&optional hide-remote)
  "Return an alist with only files and directories."
  (loop
     with r = (bookmark-region-alist-only)
     with g = (bookmark-gnus-alist-only)
     with w = (bookmark-w3m-alist-only)
     with d = (bookmark-info-alist-only)
     with rem = (bookmark-remote-alist-only)
     for i in bookmark-alist
     for pred = (if hide-remote
                    (or (member i r) (member i g) (member i w) (member i d) (member i rem))
                    (or (member i r) (member i g) (member i w) (member i d)))
     unless pred collect i))

;;;###autoload
(defun bookmark-bmenu-list-only-files-entries ()
  "Return only files and directories entries of `bookmark-alist'."
  (interactive)
  (let ((bookmark-alist (bookmark-vanilla-alist-only current-prefix-arg)))
    (call-interactively #'(lambda ()
                            (interactive)
                            (bookmark-bmenu-list "% Bookmark+ Files&Directories")))))

;;;###autoload
(defun bookmark-bmenu-list-only-info-entries ()
  "Return only Info entries of `bookmark-alist'."
  (interactive)
  (let ((bookmark-alist (bookmark-info-alist-only)))
    (call-interactively #'(lambda ()
                            (interactive)
                            (bookmark-bmenu-list "% Bookmark+ Info")))))

;;;###autoload
(defun bookmark-bmenu-list-only-w3m-entries ()
  "Return only w3m entries of `bookmark-alist'."
  (interactive)
  (let ((bookmark-alist (bookmark-w3m-alist-only)))
    (call-interactively #'(lambda ()
                            (interactive)
                            (bookmark-bmenu-list "% Bookmark+ W3m")))))

;;;###autoload
(defun bookmark-bmenu-list-only-gnus-entries ()
  "Return only gnus entries of `bookmark-alist'."
  (interactive)
  (let ((bookmark-alist (bookmark-gnus-alist-only)))
    (call-interactively #'(lambda ()
                            (interactive)
                            (bookmark-bmenu-list "% Bookmark+ Gnus")))))

;;;###autoload
(defun bookmark-bmenu-list-only-regions ()
  "Return only the elements of `bookmark-alist' that have a recorded region."
  (interactive)
  (let ((bookmark-alist (bookmark-region-alist-only)))
    (call-interactively #'(lambda ()
                            (interactive)
                            (bookmark-bmenu-list "% Bookmark+ Regions")))))

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

;; Record functions
(defun bookmark-region-record-front-context-string (breg ereg)
  "Return the region prefix, at BREG.
Return at most `bookmark-region-search-size' or (- EREG BREG) chars."
  (buffer-substring-no-properties
   breg (+ breg (min bookmark-region-search-size (- ereg breg)))))

(defun bookmark-record-front-context-string (breg)
  "Return `bookmark-search-size' chars at position BREG.
Return nil if there are not that many chars."
  (and (>= (- (point-max) breg) bookmark-search-size)
       (buffer-substring-no-properties breg (+ breg bookmark-search-size))))
      
(defun bookmark-region-record-rear-context-string (breg)
  "Return the text preceding the region beginning, BREG.
Return at most `bookmark-region-search-size' chars."
  (buffer-substring-no-properties
   (max (- breg bookmark-region-search-size) (point-min))
   breg))

(defun bookmark-record-rear-context-string (breg)
  "Return `bookmark-search-size' chars preceding BREG (inclusive).
Return nil if there are not that many chars."
  (and (>= (- breg (point-min)) bookmark-search-size)
       (buffer-substring-no-properties breg (- breg bookmark-search-size))))
  
(defun bookmark-record-front-context-region-string (breg ereg)
  "Return the region suffix, ending at EREG.
Return at most `bookmark-region-search-size' or (- EREG BREG) chars."
  (buffer-substring-no-properties
   (- ereg (min bookmark-region-search-size (- ereg breg)))
   ereg))

(defun bookmark-record-end-context-region-string (ereg)
  "Return the text following the region end, EREG.
Return at most `bookmark-region-search-size' chars."
  (buffer-substring-no-properties
   ereg (+ ereg (min bookmark-region-search-size (- (point-max) (point))))))

(defun bookmark-position-after-whitespace (position)
  "Move forward from POSITION, skipping over whitespace.  Return point."
  (goto-char position)
  (skip-chars-forward " \n\t" (point-max))
  (point))

(defun bookmark-position-before-whitespace (position)
  "Move backward from POSITION, skipping over whitespace.  Return point."
  (goto-char position)
  (skip-chars-backward " \n\t" (point-min))
  (point))

(defun bookmark-save-new-region-location (bmk-obj beg end)
  "Update and save `bookmark-alist' after relocating a region."
  (and bookmark-save-new-location-flag
       (y-or-n-p "Region relocated: Do you want to save new region limits?")
       (progn
         (bookmark-prop-set bmk-obj 'front-context-string
                            (bookmark-region-record-front-context-string beg end))
         (bookmark-prop-set bmk-obj 'rear-context-string
                            (bookmark-region-record-rear-context-string beg))
         (bookmark-prop-set bmk-obj 'front-context-region-string
                            (bookmark-record-front-context-region-string beg end))
         (bookmark-prop-set bmk-obj 'rear-context-region-string
                            (bookmark-record-end-context-region-string end))
         (bookmark-prop-set bmk-obj 'position beg)
         (bookmark-prop-set bmk-obj 'end-position end))
       t))

(defun bookmark-relocate-region-default (bmk-obj)
  "Relocate the region bookmark BMK-OBJ, by relocating the region limits.
Relocate by searching from the beginning (and possibly the end) of
the buffer."
  (let ((bor-str          (bookmark-get-front-context-string bmk-obj))
        (eor-str          (bookmark-prop-get bmk-obj 'front-context-region-string))
        (br-str           (bookmark-get-rear-context-string bmk-obj))
        (ar-str           (bookmark-get-rear-context-string bmk-obj))
        (pos              (bookmark-get-position bmk-obj))
        (end-pos          (bookmark-prop-get bmk-obj 'end-position))
        (reg-retrieved-p  t)
        (reg-relocated-p  nil))
    (unless (and (string= bor-str (buffer-substring-no-properties
                                   (point) (+ (point) (length bor-str))))
                 (save-excursion
                   (goto-char end-pos)
                   (string= eor-str (buffer-substring-no-properties
                                     (point) (- (point) (length bor-str))))))
      (let ((beg  nil)
            (end  nil))
        ;;  Go to bob and search forward for END.
        (goto-char (point-min))
        (if (search-forward eor-str (point-max) t) ; Find END, using `eor-str'.
            (setq end  (point))
            (unless (search-forward br-str (point-max) t) ; Verify if region is not before context.
              (when (search-forward ar-str (point-max) t) ; Find END, using `ar-str'.
                (setq end  (match-beginning 0)
                      end  (and end (bookmark-position-before-whitespace end))))))
        ;; If failed to find END, go to eob and search backward for BEG.
        (unless end (goto-char (point-max)))
        (if (search-backward bor-str (point-min) t) ; Find BEG, using `bor-str'.
            (setq beg  (point))
            (unless (search-backward ar-str (point-min) t) ; Verify if region is not after context.
              (when (search-backward br-str (point-min) t) ; Find BEG, using `br-str'.
                (setq beg (match-end 0)
                      beg  (and beg (bookmark-position-after-whitespace beg))))))
        (setq reg-retrieved-p  (or beg end)
              reg-relocated-p  reg-retrieved-p
              ;; If only one of BEG or END was found, the relocated region is only
              ;; approximate (keep the same length).  If both were found, it is exact.
              pos              (or beg  (and end (- end (- end-pos pos)))  pos)
              end-pos          (or end  (and beg (+ pos (- end-pos pos)))  end-pos))))

    (cond (reg-retrieved-p              ; Region is available. Activate it and maybe save it.
           (goto-char pos)
           (push-mark end-pos 'nomsg 'activate)
           (setq deactivate-mark  nil)
           (if (and reg-relocated-p (bookmark-save-new-region-location bmk-obj pos end-pos))
               (message "Saved relocated region (from %d to %d)" pos end-pos)
               (message "Region is from %d to %d" pos end-pos)))
          (t                            ; No region.  Go to old start.  Don't push-mark.
           (goto-char pos) (forward-line 0)
           (message "No region from %d to %d" pos end-pos)))))

(defun bookmark-goto-position (file buf bufname pos forward-str behind-str)
  "Go to a bookmark that has no region."
  (if (and file (file-readable-p file) (not (buffer-live-p buf)))
      (with-current-buffer (find-file-noselect file) (setq buf  (buffer-name)))
    ;; No file found.  See if a non-file buffer exists for this.  If not, raise error.
    (unless (or (and buf (get-buffer buf))
                (and bufname (get-buffer bufname) (not (string= buf bufname))))
      (signal 'file-error `("Jumping to bookmark" "No such file or directory" file))))
  (set-buffer (or buf bufname))
  (setq deactivate-mark  t)
  (raise-frame)
  (goto-char pos)
  ;; Try to relocate position.
  ;; Search forward first.
  ;; Then, if FORWARD-STR exists and was found in the file, search backward for BEHIND-STR.
  ;; The rationale is that if text was inserted between the two in the file, then it's better
  ;; to end up before point, so you can see the text, rather than after it and not see it.
  (when (and forward-str (search-forward forward-str (point-max) t))
    (goto-char (match-beginning 0)))
  (when (and behind-str (search-backward behind-str (point-min) t))
    (goto-char (match-end 0)))
  nil);; @@@@@@@@ FIXME LATER: Why do we (and vanilla Emacs) return nil?

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
      (if (and (not no-overwrite)
               (condition-case nil
                   (bookmark-get-bookmark stripped-name)
                 (error nil)))
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
         (isdired  (car (rassq (current-buffer) dired-buffers)))
         (beg      (if isregion (region-beginning) (point)))
         (end      (if isregion (region-end) (point)))
         (buf      (buffer-name))
         (fcs      (if isregion
                       (bookmark-region-record-front-context-string beg end)
                     (bookmark-record-front-context-string beg)))
         (rcs      (if isregion
                       (bookmark-region-record-rear-context-string beg)
                     (bookmark-record-rear-context-string beg)))
         (fcrs     (when isregion (bookmark-record-front-context-region-string beg end)))
         (ecrs     (when isregion (bookmark-record-end-context-region-string end))))
    `(,@(unless point-only `((filename . ,(cond ((buffer-file-name (current-buffer))
                                                 (bookmark-buffer-file-name))
                                                (isdired)
                                                (t
                                                 nil)))))
        (buffer-name . ,buf)
        (front-context-string . ,fcs)
        (rear-context-string . ,rcs)
        (front-context-region-string . ,fcrs)
        (rear-context-region-string . ,ecrs)
        (position . ,beg)
        (end-position . ,end))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Support regions and buffer names.
;;
(defun bookmark-default-handler (bmk)
  "Default handler to jump to a bookmark location.
BMK is a bookmark record.  Return nil or signal `file-error'."
  (let* ((file                   (bookmark-get-filename bmk))
         (buf                    (bookmark-prop-get bmk 'buffer))
         (bufname                (bookmark-prop-get bmk 'buffer-name))
         (pos                    (bookmark-get-position bmk))
         (end-pos                (bookmark-prop-get bmk 'end-position)))
    (if (not (and bookmark-use-region-flag end-pos (/= pos end-pos)))
        ;; Single-position bookmark (no region).  Go to it.
        (bookmark-goto-position file buf bufname pos
                                (bookmark-get-front-context-string bmk)
                                (bookmark-get-rear-context-string bmk))
        ;; Bookmark with a region.  Go to it and select region.
        ;; Get buffer.
        (if (and file (file-readable-p file) (not (buffer-live-p buf)))
            (with-current-buffer (find-file-noselect file) (setq buf  (buffer-name)))
            ;; No file found.  If no buffer either, then signal that file doesn't exist.
            (unless (or (and buf (get-buffer buf))
                        (and bufname (get-buffer bufname) (not (string= buf bufname))))
              (signal 'file-error `("Jumping to bookmark" "No such file or directory"
                                                          (bookmark-get-filename bmk)))))
        (set-buffer (or buf bufname))
        (raise-frame)
        (goto-char (min pos (point-max)))
        (when (> pos (point-max)) (error "Bookmark position is beyond buffer end"))
        ;; Relocate region if it has moved.
        (funcall bookmark-relocate-region-function bmk))))


;; Same as vanilla Emacs 23+ definitions.
;;
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
           (buf (save-window-excursion  ; VANILLA EMACS FIXME: doesn't work with frames!
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

(defun bookmark-w3m-set-new-buffer-name ()
  "Set the w3m buffer name according to the number of w3m buffers already open."
  (let ((len (length (w3m-list-buffers 'nosort))))
    (if (eq len 0)
        "*w3m*"
        (format "*w3m*<%d>" (1+ len)))))

(defun bookmark-jump-w3m-new-session (bmk)
  "Jump to bookmark in w3m setting a new tab."
  (let ((file (bookmark-prop-get bmk 'filename))
        (buf  (bookmark-w3m-set-new-buffer-name)))
    (w3m-browse-url file 'newsession)
    ;; Be sure we have our w3m buffer.
    (while (not (get-buffer buf))
      (sit-for 1))
    (with-current-buffer buf
      (goto-char (point-min))
      ;; Wait data arrive in buffer to set region.
      (while (eq (line-beginning-position) (line-end-position)) (sit-for 1)))
    (bookmark-default-handler
     (list* "" `(buffer . ,buf)
            (bookmark-get-bookmark-record bmk)))))

(defun bookmark-jump-w3m-only-one-tab (bmk)
  "Close all w3m sessions and jump to bookmark `bmk' in new w3m buffer."
  (let ((file  (bookmark-prop-get bmk 'filename)))
    (w3m-quit 'force) ; Be sure we start on an empty w3m buffer. 
    (w3m-browse-url file)
    (with-current-buffer "*w3m*" (while (eq (point-min) (point-max)) (sit-for 1)))
    (bookmark-default-handler
     (list* "" `(buffer . ,(buffer-name (current-buffer)))
            (bookmark-get-bookmark-record bmk)))))

(defun bookmark-jump-w3m (bmk)
  "Handler function for record returned by `bookmark-make-w3m-record'.
Use multi tabs in w3m according if `bookmark-w3m-allow-multi-tabs' is non--nil."
  (if bookmark-w3m-allow-multi-tabs
      (bookmark-jump-w3m-new-session bmk)
      (bookmark-jump-w3m-only-one-tab bmk)))

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
    `(,@(bookmark-make-record-default 'point-only) (group . ,grp) (article . ,art)
      (message-id . ,id) (handler . bookmark-jump-gnus))))

(add-hook 'gnus-summary-mode-hook
          #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                            'bookmark-make-gnus-record)))

;; Raise an error if we try to bookmark from here [1]
(add-hook 'gnus-article-mode-hook
          #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
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
    (bookmark-default-handler
     (list* "" `(buffer . ,buf) (bookmark-get-bookmark-record bmk)))))

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

(provide 'bookmark+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark+.el ends here
