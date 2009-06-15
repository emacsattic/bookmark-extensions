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
;;   `bookmark-rename', `bookmark-location', `bookmark-make-record',
;;   `bookmark-handle-bookmark', `bookmark-bmenu-list'.
;;
;;  * New functions:
;;    `bookmark-region-handler', `bookmark-make-record-region',
;;    `bookmark-get-buffername'.
;;
;;  * User variables:
;;    `bookmark-use-region', `bookmark-region-search-size'.
;;
;;  * New features:
;;    - Support for bookmarking region of text.
;;    - Support bookmarking non--buffer-file-name when using region.
;;    - Support bookmarking regions in w3m buffers.
;;
;;  * Usage:
;;    Put this file in your load path
;;    Add to .emacs : (require 'bookmark+)
;;    If you don't like bookmark active regions when jumping to bookmarks,
;;    Add to .emacs: (setq bookmark-use-region nil)
;;    Even if `bookmark-use-region' is nil, you can active the region
;;    (if this bookmark have one) with a prefix argument (C-u RET) when jumping
;;    to bookmark.
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
(eval-when-compile (require 'cl))

(defconst bookmark+version-number "1.1.1")

(defun bookmark+version ()
  "Show version number of bookmark+.el"
  (interactive)
  (message "Bookmark+ Version %s" bookmark+version-number))

;;;;;;;;;;;;;;;;;;;;;;

;;; keymap
;; Prefix is C-x p
;; Commands are run with C-x p <command> (e.g "C-x p T")

;;;###autoload
(define-key ctl-x-map "p" bookmark-map)
;;;###autoload
(define-key bookmark-map "T" 'bookmark-toggle-only-regions)

;;; User variables
(defvar bookmark-use-region t
  "When non--nil active region if one have been saved
when jumping to bookmark.")

(defvar bookmark-region-search-size 40
  "The same as `bookmark-search-size' but specialized for bookmark regions.")

;;; Faces
(defface bookmark-file-name-face
  '((t (:foreground "Blue")))
  "*Face used for file names in bookmark buffer."
  :group 'bookmark)

(defface bookmark-directory-name-face
  '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directory names in bookmark buffer."
  :group 'bookmark)

(defface bookmark-w3m-url-face
  '((t (:foreground "yellow")))
  "*Face used for w3m urls in bookmark buffer."
  :group 'bookmark)

(defface bookmark-info-buffer-face
  '((t (:foreground "green")))
  "*Face used for info buffers in bookmark buffer."
  :group 'bookmark)

(defface bookmark-buffer-nonfile-name-face
  '((t (:foreground "grey")))
  "*Face used for buffer nonfile-names in bookmark buffer."
  :group 'bookmark)

(defface bookmark-buffer-file-name-region-face
  '((t (:foreground "Indianred2")))
  "*Face used for buffer nonfile-names in bookmark buffer."
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
  (if current-prefix-arg
      (let ((bookmark-use-region (not bookmark-use-region)))
        (old-bookmark-jump bookmark))
      (old-bookmark-jump bookmark)))

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
       ;; if a bookmark has an annotation, prepend a "*"
       ;; in the list of bookmarks.
       (let ((annotation (bookmark-get-annotation
                          (bookmark-name-from-full-record full-record))))
         (if (and annotation (not (string-equal annotation "")))
             (insert " *")
           (insert "  "))
	 (let* ((start (point))
                (name (bookmark-name-from-full-record full-record))
                (isfile (bookmark-get-filename name))
                (istramp (when isfile
                           (tramp-tramp-file-p isfile)))
                (isregion (and (bookmark-get-endposition name)
                               (/= (bookmark-get-position name)
                                   (bookmark-get-endposition name))))
                (isannotation (bookmark-get-annotation name))
                (ishandler (bookmark-get-handler name))
                (isbuf (bookmark-get-buffername name)))
	   (insert name)
	   (if (and (display-color-p) (display-mouse-p))
	       (add-text-properties
		start
		(save-excursion (re-search-backward
				 "[^ \t]")
				(1+ (point)))
                (cond ((and isfile ;; dirs
                            (not istramp)
                            (file-directory-p isfile))
                       '(mouse-face highlight
                         follow-link t
                         face 'bookmark-directory-name-face
                         help-echo "mouse-2: go to this dired buffer in other window"))
                      ((and isfile ;; regular files with region
                            (not istramp)
                            (not (file-directory-p isfile))
                            (file-exists-p isfile)
                            isregion)
                         '(mouse-face highlight
                           follow-link t
                           face 'bookmark-buffer-file-name-region-face
                           help-echo "mouse-2: go to this file with region"))
                      ((and isfile ;; regular files
                            (not istramp)
                            (not (file-directory-p isfile))
                            (file-exists-p isfile))
                       '(mouse-face highlight
                         follow-link t
                         face 'bookmark-file-name-face
                         help-echo "mouse-2: go to this file in other window"))
                      ((and isbuf ;; buffers non--filename
                            (not isfile))
                         '(mouse-face highlight
                           follow-link t
                           face 'bookmark-buffer-nonfile-name-face 
                           help-echo "mouse-2: go to this non--buffer-filename"))
                      ((and (string= isbuf "*w3m*") ;; w3m urls
                            (when isfile
                              (not (file-exists-p isfile))))
                       '(mouse-face highlight
                         follow-link t
                         face 'bookmark-w3m-url-face
                         help-echo "mouse-2: go to this w3m url"))
                      ((when (or ;; info buffers
                              (eq ishandler 'Info-bookmark-jump)
                              (and (string= isbuf "*info*")
                                   (when isfile
                                     (not (file-exists-p isfile)))))
                         '(mouse-face highlight
                           follow-link t
                           face 'bookmark-info-buffer-face
                           help-echo "mouse-2: go to this info buffer")))
                      (istramp
                       '(mouse-face highlight
                         follow-link t
                         face 'italic
                         help-echo "mouse-2: go to this tramp buffer")))))
	   (insert "\n")
	   )))
     (bookmark-maybe-sort-alist)))
  (goto-char (point-min))
  (forward-line 2)
  (bookmark-bmenu-mode)
  (if bookmark-bmenu-toggle-filenames
      (bookmark-bmenu-toggle-filenames t)))

(defun bookmark-get-buffername (bookmark)
  "Return the buffer-name of BOOKMARK."
  (bookmark-prop-get bookmark 'buffer-name))

(defun bookmark-get-endposition (bookmark)
  "Return the end-position of REGION in BOOKMARK."
  (bookmark-prop-get bookmark 'end-position))

(defun bookmark-region-alist-only ()
  "Create an alist with only bookmarks with region."
  (loop for i in bookmark-alist
     for b = (and (bookmark-get-endposition i)
                  (/= (bookmark-get-position i)
                      (bookmark-get-endposition i)))
     if b
     collect i))

(defun bookmark-region-alist-only-names ()
  "Similar at `bookmark-all-names' but with only bookmarks with regions."
  (loop with alist = (bookmark-region-alist-only)
     for i in alist
     collect (car i)))

(defvar bookmark-list-only-regions-flag t)
(defun bookmark-list-only-regions ()
  (let ((bookmark-alist (bookmark-region-alist-only)))
    (call-interactively #'bookmark-bmenu-list)))

;;;###autoload
(defun bookmark-toggle-only-regions ()
  (interactive)
  (if bookmark-list-only-regions-flag
      (progn
        (bookmark-list-only-regions)
        (setq bookmark-list-only-regions-flag nil))
      (call-interactively #'bookmark-bmenu-list)
      (setq bookmark-list-only-regions-flag t)))

;; (find-fline "~/download/bookmark+-2009-06-13a-DREW.el" "defun bookmark-location")
;; TODO Replace with Drew version:
;; (find-fline "~/download/bookmark+-2009-06-13a-DREW.el" "defun bookmark-location")
(defun bookmark-location (bookmark)
  "Return the name of the file or buffer associated with BOOKMARK."
  (bookmark-maybe-load-default-file)
  (or (bookmark-get-filename bookmark)
      (bookmark-get-buffername bookmark)
      "*What is this?*"))

;; (find-fline "/usr/share/emacs/23.0.94/lisp/bookmark.el" "defun bookmark-make-record")
;; (find-fline "/usr/share/emacs/23.0.94/lisp/bookmark.el" "defun bookmark-make-record-default")
;; (find-fline "/usr/share/emacs/23.0.94/lisp/info.el" "defun Info-bookmark-make-record")

;; That do not handle anymore info nor w3m
(defun bookmark-make-record-default (&optional point-only)
  "Return the record describing the location of a new bookmark.
Must be at the correct position in the buffer in which the bookmark is
being set.
If POINT-ONLY is non-nil, then only return the subset of the
record that pertains to the location within the buffer."
  (let* ((isregion (and transient-mark-mode
                        (region-active-p)
                        (not (eq (mark) (point)))))
         (beg (if isregion
                  (region-beginning)
                  (point)))
         (end (if isregion
                  (region-end)
                  (point)))
         (buf (buffer-name))
         (fcs (if isregion
                  (buffer-substring-no-properties
                   beg
                   (+ beg (min bookmark-region-search-size (- end beg))))
                  (if (>= (- (point-max) (point)) bookmark-search-size)
                      (buffer-substring-no-properties
                       (point)
                       (+ (point) bookmark-search-size))
                      nil)))
         (ecs (if isregion
                  (buffer-substring-no-properties
                   end
                   (- end (min bookmark-region-search-size
                               (- end beg))))
                  (if (>= (- (point) (point-min)) bookmark-search-size)
                      (buffer-substring-no-properties
                       (point)
                       (- (point) bookmark-search-size))
                      nil))))
    `(,@(unless point-only `((filename . ,(cond ((buffer-file-name (current-buffer))
                                                 (bookmark-buffer-file-name))
                                                (t
                                                 nil)))))
        (buffer
         . ,buf)
        (front-context-string . ,fcs)
        (rear-context-string . ,ecs)
        (position . ,beg)
        (end-position . ,end))))


;; (find-fline "/usr/share/emacs/23.0.94/lisp/bookmark.el" "defun bookmark-default-handler")
;; Redefine `bookmark-default-handler' with support for region
(defun bookmark-default-handler (bmk)
  (let* ((file                   (bookmark-get-filename bmk))
         (buf                    (bookmark-prop-get bmk 'buffer))
         (forward-str            (bookmark-get-front-context-string bmk))
         (behind-str             (bookmark-get-rear-context-string bmk))
         (place                  (bookmark-get-position bmk))
         (end-pos                (bookmark-prop-get bmk 'end-position))
         (region-retrieved-p     t))
    (if (and end-pos
             (/= place end-pos))
        ;; A saved region exists retrieve it
        (progn
          (cond ((and file ;; file exists and is readable
                      (file-readable-p file))
                    (find-file-noselect file))
                 (t
                  ;; No file found we search for a buffer non--filename
                  ;; if not found, signal file doesn't exist anymore
                  (if (not (get-buffer buf))
                      (signal 'file-error
                              `("Jumping to bookmark" "No such file or directory"
                                                   (bookmark-get-filename bmk))))))
          (when (get-buffer buf)
            (pop-to-buffer buf)
            (raise-frame)
            (goto-char place)      
            ;; Check if start of region have moved
            (unless (and ;(not (eq place end-pos))
                         (string= forward-str (buffer-substring-no-properties (point) (+ (point) (length forward-str))))
                         (save-excursion
                           ;; check also if end of region have changed
                           (goto-char end-pos)
                           (string= behind-str (buffer-substring-no-properties (point) (- (point) (length forward-str))))))
              ;; Position have changed: relocate region.
              (goto-char (point-max))
              (let (beg end)
                (when (re-search-backward (regexp-opt (list forward-str) t) nil t)
                  (setq beg (point))
                  (save-excursion
                    (when (re-search-forward (regexp-opt (list behind-str) t) nil t)
                      (setq end (point)))))
                ;; Save new location to `bookmark-alist'.
                (if (and beg end)
                    (progn
                      (setq place beg
                            end-pos end)
                      (setf (cdr (assoc 'position bmk)) place)
                      (setf (cdr (assoc 'end-position bmk)) end-pos))
                    (setq region-retrieved-p nil)))))
          ;; Region found
          (if region-retrieved-p
              (progn
                (push-mark end-pos 'nomsg 'activate)
                (setq deactivate-mark  nil)
                (message "Region at Start:%s to End:%s" place end-pos))
              ;; Region doesn't exist anymore, go at old start pos
              ;; and don't push-mark.
              (goto-char place) (beginning-of-line)
              (message "Region at Start:%s to End:%s not found!" place end-pos)))
        ;; There is no saved region, retrieve file as normal.
        (cond ((when (and file
                          (file-readable-p file))
                 (find-file-noselect (expand-file-name file))
                 (unless buf
                   (if (file-directory-p file)
                       (setq buf (file-name-nondirectory (directory-file-name file)))
                       (setq buf (file-name-nondirectory file))))))
              (t
               ;; No file found we search for a buffer non--filename
               ;; if not found signal file doesn't exist anymore
               (if (not (get-buffer buf))
                   (signal 'file-error
                           `("Jumping to bookmark" "No such file or directory"
                                                   (bookmark-get-filename bmk))))))
        (when (get-buffer buf)
          (pop-to-buffer buf)
          (setq deactivate-mark  t)
          (raise-frame)
          (goto-char place)
          ;; Go searching forward first.  Then, if forward-str exists and
          ;; was found in the file, we can search backward for behind-str.
          ;; Rationale is that if text was inserted between the two in the
          ;; file, it's better to be put before it so you can read it,
          ;; rather than after and remain perhaps unaware of the changes.
          (if forward-str
              (if (search-forward forward-str (point-max) t)
                  (goto-char (match-beginning 0))))
          (if behind-str
              (if (search-backward behind-str (point-min) t)
                  (goto-char (match-end 0)))))
        nil)))


;; (when (re-search-forward "^.*[^ \n]") (beginning-of-line))
;; (find-fline "/usr/share/emacs/23.0.94/lisp/info.el" "defun Info-bookmark-jump")
;; (find-fline "~/download/bookmark+-2009-06-13a-DREW.el" "defun Info-bookmark-jump")
;; (find-fline "/usr/share/emacs/23.0.94/lisp/bookmark.el" "defun bookmark-default-handler")
;; (find-fline "/usr/share/emacs/23.0.94/lisp/bookmark.el" "defun bookmark-handle-bookmark")


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
