;;; bookmark+.el - Extensions to standard library `bookmark.el'.

;; Filename: bookmark+.el

;; Author: Drew Adams, Thierry Volpiatto
;; Maintainer: Thierry Volpiatto

;; Copyright (C) 2000-2009, Drew Adams, all rights reserved.
;; Copyright (C) 2009, Thierry Volpiatto, all rights reserved.

;; Created: Fri Sep 15 07:58:41 2000

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

;; X-URL: http://mercurial.intuxication.org/hg/emacs-bookmark-extension/

;; Keywords: bookmarks, placeholders, annotations, search, info, w3m, gnus

;; Compatibility: GNU Emacs: 23.x

;; Features that might be required by this library:

;;   `bookmark', `ffap', `pp'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to standard library `bookmark.el'.
;;
;;    More description below.
;;
;;  Things Defined Here
;;  -------------------

;;  [UPDATE ALL EVAL] (traverse-auto-update-documentation)

;;  * Commands defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'command :prefix "bookmarkp")
;; `bookmarkp-version'
;; `bookmarkp-bmenu-sort-by-visit-frequency'
;; `bookmarkp-bmenu-sort-by-last-time-visited'
;; `bookmarkp-bmenu-sort-alphabetically'
;; `bookmarkp-bmenu-search'
;; `bookmarkp-bmenu-edit-bookmark'
;; `bookmarkp-bmenu-delete-bookmark'
;; `bookmarkp-bmenu-quit'
;; `bookmarkp-bmenu-list-only-file-bookmarks'
;; `bookmarkp-bmenu-list-only-non-file-bookmarks'
;; `bookmarkp-bmenu-list-only-info-bookmarks'
;; `bookmarkp-bmenu-list-only-w3m-bookmarks'
;; `bookmarkp-bmenu-list-only-gnus-bookmarks'
;; `bookmarkp-bmenu-list-only-region-bookmarks'
;; `bookmarkp-bmenu-list-only-woman-man-bookmarks'
;; `bookmarkp-bmenu-show-all-bookmarks'
;; `bookmarkp-bmenu-mark-all-bookmarks'
;; `bookmarkp-bmenu-unmark-all-deletion-flags'
;; `bookmarkp-bmenu-unmark-all-non-deletion-flags'
;; `bookmarkp-bmenu-unmark-all'
;; `bookmarkp-bmenu-regexp-mark'
;; `bookmarkp-bmenu-hide-marked'
;; `bookmarkp-bmenu-hide-unmarked'
;; `bookmarkp-bmenu-toggle-marks'

;;  * Commands redefined here:(from `bookmark.el')
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'command :prefix "^bookmark-")
;; `bookmark-set'
;; `bookmark-yank-word'
;; `bookmark-bmenu-mark'
;; `bookmark-bmenu-unmark'
;; `bookmark-bmenu-this-window'
;; `bookmark-jump'
;; `bookmark-jump-other-window'
;; `bookmark-rename'
;; `bookmark-insert'
;; `bookmark-delete'
;; `bookmark-bmenu-list'
;; `bookmark-bmenu-other-window'
;; `bookmark-bmenu-execute-deletions'
;; `bookmark-bmenu-rename'

;;  * User options defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'user-variable)
;; `bookmarkp-use-region-flag'
;; `bookmarkp-region-search-size'
;; `bookmarkp-save-new-location-flag'
;; `bookmarkp-handle-region-function'
;; `bookmarkp-su-or-sudo-regexp'
;; `bookmarkp-w3m-allow-multi-tabs'
;; `bookmarkp-show-end-of-region'
;; `bookmarkp-bookmark-name-length-max'
;; `bookmarkp-bmenu-sort-function'
;; `bookmarkp-search-prompt'
;; `bookmarkp-search-delay'
;; `bookmarkp-local-man-name-regexp'

;;  * Faces defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'faces)
;; `bookmarkp-gnus'
;; `bookmarkp-info'
;; `bookmarkp-local-directory'
;; `bookmarkp-local-file-with-region'
;; `bookmarkp-local-file-without-region'
;; `bookmarkp-non-file'
;; `bookmarkp-remote-file'
;; `bookmarkp-su-or-sudo'
;; `bookmarkp-w3m'
;; `bookmarkp-woman'

;;  * Non-interactive functions defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'function :prefix "bookmarkp")
;; `bookmarkp-maybe-save-bookmark'
;; `bookmarkp-edit-bookmark'
;; `bookmarkp-increment-visits'
;; `bookmarkp-add-or-update-time'
;; `bookmarkp-update-time-and-increment-visits'
;; `bookmarkp-sort-p-1'
;; `bookmarkp-bmenu-maybe-sort'
;; `bookmarkp-bmenu-sort-1'
;; `bookmarkp-bmenu-goto-bookmark-named'
;; `bookmarkp-read-search-input'
;; `bookmarkp-filtered-alist-by-regexp-only'
;; `bookmarkp-bmenu-filter-alist-by-regexp'
;; `bookmarkp-bmenu-cancel-search'
;; `bookmarkp-bmenu-propertize-item'
;; `bookmarkp-bmenu-unmark-all-1'
;; `bookmarkp-bmenu-unmark-all-2'
;; `bookmarkp-count-marked'
;; `bookmarkp-region-bookmark-p'
;; `bookmarkp-gnus-bookmark-p'
;; `bookmarkp-w3m-bookmark-p'
;; `bookmarkp-info-bookmark-p'
;; `bookmarkp-woman-bookmark-p'
;; `bookmarkp-man-bookmark-p'
;; `bookmarkp-woman-man-bookmark-p'
;; `bookmarkp-file-bookmark-p'
;; `bookmarkp-non-file-bookmark-p'
;; `bookmarkp-remote-file-bookmark-p'
;; `bookmarkp-local-file-bookmark-p'
;; `bookmarkp-local-directory-bookmark-p'
;; `bookmarkp-bookmark-marked-p'
;; `bookmarkp-region-alist-only'
;; `bookmarkp-gnus-alist-only'
;; `bookmarkp-w3m-alist-only'
;; `bookmarkp-info-alist-only'
;; `bookmarkp-woman-alist-only'
;; `bookmarkp-man-alist-only'
;; `bookmarkp-woman-man-alist-only'
;; `bookmarkp-remote-file-alist-only'
;; `bookmarkp-local-file-alist-only'
;; `bookmarkp-file-alist-only'
;; `bookmarkp-non-file-alist-only'
;; `bookmarkp-marked-bookmarks-only'
;; `bookmarkp-non-marked-bookmarks-only'
;; `bookmarkp-current-list-have-marked-p'
;; `bookmarkp-get-buffer-name'
;; `bookmarkp-get-end-position'
;; `bookmarkp-root-or-sudo-logged-p'
;; `bookmarkp-region-record-front-context-string'
;; `bookmarkp-record-front-context-string'
;; `bookmarkp-region-record-rear-context-string'
;; `bookmarkp-record-rear-context-string'
;; `bookmarkp-record-front-context-region-string'
;; `bookmarkp-record-end-context-region-string'
;; `bookmarkp-position-after-whitespace'
;; `bookmarkp-position-before-whitespace'
;; `bookmarkp-save-new-region-location'
;; `bookmarkp-handle-region-default'
;; `bookmarkp-goto-position'
;; `bookmarkp-make-w3m-record'
;; `bookmarkp-w3m-set-new-buffer-name'
;; `bookmarkp-jump-w3m-new-session'
;; `bookmarkp-jump-w3m-only-one-tab'
;; `bookmarkp-jump-w3m'
;; `bookmarkp-make-gnus-record'
;; `bookmarkp-jump-gnus'
;; `bookmarkp-make-woman-record'
;; `bookmarkp-jump-woman'
;; `bookmarkp-make-man-record'
;; `bookmarkp-jump-man'

;;  * Non-interactive functions redefined here:(From `bookmark.el')
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'function :prefix "^bookmark-")
;; `bookmark-bmenu-mode'
;; `bookmark-make-record-default'
;; `bookmark-bmenu-bookmark'
;; `bookmark-bmenu-check-position'
;; `bookmark--jump-via'
;; `bookmark-prop-set'
;; `bookmark-default-handler'
;; `bookmark-location'
;; `bookmark-write-file'
;; `bookmark-bmenu-surreptitiously-rebuild-list'
;; `bookmark-bmenu-hide-filenames'

;;  * Internal variables defined here:
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'internal-variable :prefix "bookmarkp")
;; `bookmarkp-jump-display-function'
;; `bookmarkp-latest-bookmark-alist'
;; `bookmarkp-bookmark-marked-list'
;; `bookmarkp-bmenu-before-hide-unmarked-list'
;; `bookmarkp-bmenu-before-hide-marked-list'
;; `bookmarkp-bmenu-called-from-inside-flag'
;; `bookmarkp-bmenu-reverse-sort-p'
;; `bookmarkp-search-pattern'
;; `bookmarkp-search-timer'

;;  ***** NOTE: The following variables defined in `bookmark.el'
;;              have been REDEFINED HERE.
;; [EVAL] (traverse-auto-document-lisp-buffer :type 'internal-variable :prefix "^bookmark-")
;; `bookmark-make-record-function'
;; `bookmark-alist'

;;  *** END auto-documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Documentation
;;  -------------
;;
;;  ** Bookmark+ Features **
;;
;;  In addition to the kinds of bookmarks provided by vanilla Emacs:
;;
;;    - You can bookmark a buffer that is not associated with a file.
;;
;;    - You can bookmark a region in a W3m buffer.
;;
;;    - For any bookmark (except Gnus), you can bookmark a region of
;;      text, not just a position.  By default, when you jump to a
;;      bookmark that records a region, the region is activated.  See
;;      option `bookmarkp-use-region-flag'.  `C-u' reverses the
;;      behavior specified by the value of the option.
;;
;;    - Better bookmark relocation, if the contextual text changes.
;;
;;
;;  ** How To Use Bookmark+ **
;;
;;  Put this library in your `load-path'.
;;  Add this to your init file (~/.emacs) : (require 'bookmark+)
;;
;;  Some of the commands defined here bind `S-delete', to delete the
;;  current bookmark candidate during completion in Icicle mode (see
;;  Icicles: http://www.emacswiki.org/cgi-bin/wiki/Icicles).
;;
;;
;;  ** Compatibility with Vanilla Emacs (`bookmark.el') **
;;
;;  1. All bookmarks created using any version of vanilla Emacs
;;     (library `bookmark.el') continue to work with `bookmark+.el'.
;;
;;  2. Most bookmarks created using `bookmark+.el' will not interfere
;;     with the behavior of vanilla Emacs, versions 23.  The new
;;     bookmark types are simply ignored by vanilla Emacs.  For
;;     example:
;;
;;    - A bookmark with a region is treated like a simple position
;;      bookmark: the destination is the region start position.
;;
;;    - A Gnus bookmark does not work; it is simply ignored.
;;
;;  However, there are two cases in which `bookmark+.el' bookmarks
;;  will raise an error in vanilla Emacs:
;;
;;  * You cannot use non-file bookmarks with any version of vanilla
;;    Emacs.
;;
;;
;;    The Emacs bookmark data structure has changed from version to
;;    version.  Library `bookmark+.el' always creates bookmarks that
;;    have the most recent structure (Emacs 23).
;;
;;  Bottom line: Use `bookmark+.el' to access bookmarks created using
;;  `bookmark+.el'.
;;
;;
;;  ** New Bookmark Structure **
;;
;;  The bookmark data structure, variable `bookmark-alist', has been
;;  enhanced to support the new bookmark types.  For a description of
;;  this enhanced structure, use `C-h v bookmark-alist'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; See the change log at: http://mercurial.intuxication.org/hg/emacs-bookmark-extension/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:

(require 'bookmark)
(unless (fboundp 'file-remote-p) (require 'ffap)) ;; ffap-file-remote-p
(eval-when-compile (require 'gnus)) ;; mail-header-id (really in `nnheader.el')
(eval-when-compile (require 'cl))


(defconst bookmarkp-version-number "2.5.61")

(defun bookmarkp-version ()
  "Show version number of library `bookmark+.el'."
  (interactive)
  (message "Bookmark+, version %s" bookmarkp-version-number))


;; Quiet the byte-compiler
(defvar w3m-current-url)                ; Defined in `w3m.el'.
(defvar w3m-current-title)              ; Defined in `w3m.el'.
(defvar gnus-article-current)           ; Defined in `gnus-sum.el'.
(defvar tramp-file-name-regexp)         ; Defined in `tramp.el'.
(defvar bookmark-make-record-function)  ; Defined in `bookmark.el'.
(defvar Info-current-node)              ; Defined in `info.el'.
(defvar Info-current-file)              ; Defined in `info.el'.


;;; Keymaps -------------------------------------------------------

;;;###autoload
(define-key ctl-x-map "p" bookmark-map)
;;;###autoload
(define-key ctl-x-map "pj" 'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "o" 'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "q" 'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "F" 'bookmarkp-bmenu-list-only-file-bookmarks)
;;;###autoload
(define-key bookmark-map "G" 'bookmarkp-bmenu-list-only-gnus-bookmarks)
;;;###autoload
(define-key bookmark-map "I" 'bookmarkp-bmenu-list-only-info-bookmarks)
;;;###autoload
(define-key bookmark-map "R" 'bookmarkp-bmenu-list-only-region-bookmarks)
;;;###autoload
(define-key bookmark-map "W" 'bookmarkp-bmenu-list-only-w3m-bookmarks)

;; *-bmenu-mode-map

;;;###autoload
(define-key bookmark-bmenu-mode-map "." 'bookmarkp-bmenu-show-all-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "U") nil)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "U <RET>") 'bookmarkp-bmenu-unmark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "M-<DEL>") 'bookmarkp-bmenu-unmark-all)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "U D") 'bookmarkp-bmenu-unmark-all-deletion-flags)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "U >") 'bookmarkp-bmenu-unmark-all-non-deletion-flags)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-m" 'bookmarkp-bmenu-mark-all-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-t" 'bookmark-bmenu-toggle-filenames) ; Was `t' in Vanilla
;;;###autoload
(define-key bookmark-bmenu-mode-map "t" 'bookmarkp-bmenu-toggle-marks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "q" 'bookmarkp-bmenu-quit)
;;;###autoload
(define-key bookmark-bmenu-mode-map "E" 'bookmarkp-bmenu-edit-bookmark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "F" 'bookmarkp-bmenu-list-only-file-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "G" 'bookmarkp-bmenu-list-only-gnus-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map ">" 'bookmarkp-bmenu-hide-unmarked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "<" 'bookmarkp-bmenu-hide-marked)
;;;###autoload
(define-key bookmark-bmenu-mode-map "I" 'bookmarkp-bmenu-list-only-info-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "M" 'bookmarkp-bmenu-list-only-woman-man-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "B" 'bookmarkp-bmenu-list-only-non-file-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "R" 'bookmarkp-bmenu-list-only-region-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\S-V" 'bookmarkp-bmenu-sort-by-visit-frequency)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\S-T" 'bookmarkp-bmenu-sort-by-last-time-visited)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\S-S" 'bookmarkp-bmenu-sort-alphabetically)
;;;###autoload
(define-key bookmark-bmenu-mode-map "\M-r" 'bookmark-bmenu-relocate) ; Was `R' in vanilla.
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "M-g") 'bookmarkp-bmenu-search)
;;;###autoload
(define-key bookmark-bmenu-mode-map (kbd "C-k") 'bookmarkp-bmenu-delete-bookmark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "W" 'bookmarkp-bmenu-list-only-w3m-bookmarks)
;;;###autoload
(define-key bookmark-bmenu-mode-map "%" nil)
;;;###autoload
(define-key bookmark-bmenu-mode-map "%m" 'bookmarkp-bmenu-regexp-mark)
;;;###autoload
(define-key bookmark-bmenu-mode-map "*" nil)
;;;###autoload
(when (< emacs-major-version 21)
  (define-key bookmark-bmenu-mode-map (kbd "RET") 'bookmark-bmenu-this-window)
  (define-key bookmark-bmenu-mode-map (kbd "*m") 'bookmark-bmenu-mark))

(defadvice bookmark-bmenu-mode (before bookmark+-add-keymap () activate)
  "Extras keys added by bookmark+:\\<bookmark-bmenu-mode-map>
\\[bookmarkp-bmenu-edit-bookmark]\t- Edit bookmark
\\[bookmarkp-bmenu-list-only-file-bookmarks]\t- List only file and directory \
bookmarks (`C-u' for local only)
\\[bookmarkp-bmenu-show-all-bookmarks]\t- Show all bookmarks
\\[bookmark-bmenu-toggle-filenames]\t- Toggle filenames
\\[bookmarkp-bmenu-toggle-marks]\t- Toggle marks
\\[bookmarkp-bmenu-list-only-non-file-bookmarks]\t- List only non-file bookmarks
\\[bookmarkp-bmenu-list-only-gnus-bookmarks]\t- List only Gnus bookmarks
\\[bookmarkp-bmenu-list-only-info-bookmarks]\t- List only Info bookmarks
\\[bookmarkp-bmenu-list-only-woman-man-bookmarks]\t- List only Woman and Man  pages
\\[bookmarkp-bmenu-list-only-region-bookmarks]\t- List only region bookmarks
\\[bookmarkp-bmenu-list-only-w3m-bookmarks]\t- List only W3M bookmarks
\\[bookmarkp-bmenu-regexp-mark]\t- Mark bookmarks that match a regexp
\\[bookmarkp-bmenu-hide-marked]\t- Hide marked bookmarks
\\[bookmarkp-bmenu-hide-unmarked]\t- Hide unmarked bookmarks
\\[bookmarkp-bmenu-mark-all-bookmarks]\t- Mark all bookmarks
\\[bookmarkp-bmenu-search]\t- Incremental search in bookmarks
\\[bookmarkp-bmenu-unmark-all]\t- Unmark all bookmarks (`C-u' for interactive use)
\\[bookmarkp-bmenu-unmark-all-non-deletion-flags]\t- Unmark all bookmarks with flag >
\\[bookmarkp-bmenu-unmark-all-deletion-flags]\t- Unmark all bookmarks with flag D
\\[bookmarkp-bmenu-sort-by-visit-frequency]\t- Sort by visit frequency (`C-u' to reverse)
\\[bookmarkp-bmenu-sort-by-last-time-visited]\t- Sort by last time visited (`C-u' to reverse)
\\[bookmarkp-bmenu-sort-alphabetically]\t- Sort alphabetically (`C-u' to reverse)")


;;; Faces (Customizable) ---------------------------------------------

(defface bookmarkp-gnus
    '((t (:foreground "magenta")))
  "*Face used for a gnus bookmark."
  :group 'bookmarkp)

(defface bookmarkp-info
    '((t (:foreground "green")))
  "*Face used for a bookmarked Info node."
  :group 'bookmarkp)

(defface bookmarkp-local-directory
    '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for a bookmarked local directory."
  :group 'bookmarkp)

(defface bookmarkp-local-file-with-region
    '((t (:foreground "Indianred2")))
  "*Face used for a region bookmark in a local file."
  :group 'bookmarkp)

(defface bookmarkp-local-file-without-region
    '((t (:foreground "Blue")))
  "*Face used for a bookmarked local file (without a region)."
  :group 'bookmarkp)

(defface bookmarkp-non-file
    '((t (:foreground "grey")))
  "*Face used for a bookmarked buffer not associated with a file."
  :group 'bookmarkp)

(defface bookmarkp-remote-file
    '((t (:foreground "pink")))
  "*Face used for a bookmarked tramp remote file (/ssh:)."
  :group 'bookmarkp)

(defface bookmarkp-su-or-sudo
    '((t (:foreground "red")))
  "*Face used for a bookmarked tramp file (/su: or /sudo:)."
  :group 'bookmarkp)

(defface bookmarkp-w3m
    '((t (:foreground "yellow")))
  "*Face used for a bookmarked w3m url."
  :group 'bookmarkp)

(defface bookmarkp-woman
    '((t (:foreground "Orange4")))
  "*Face used for a bookmarked w3m url."
  :group 'bookmarkp)


;;; User Options (Customizable) --------------------------------------

(defgroup bookmark-plus nil
  "Bookmark enhancements."
  :prefix "bookmarkp-" :group 'bookmark)

(defcustom bookmarkp-use-region-flag t
  "*Non-nil means jumping to bookmark activates bookmarked region, if any."
  :type 'boolean :group 'bookmarkp)

(defcustom bookmarkp-region-search-size 40
  "*Same as `bookmark-search-size', but specialized for bookmark regions."
  :type 'integer :group 'bookmarkp)

(defcustom bookmarkp-save-new-location-flag t
  "*Non-nil means save relocated bookmarks.
If nil, then the new bookmark location is visited, but it is not saved
as part of the bookmark definition."
  :type 'boolean :group 'bookmarkp)

(defcustom bookmarkp-handle-region-function 'bookmarkp-handle-region-default
  "*Function to handle a bookmarked region."
  :type 'function :group 'bookmarkp)

(defcustom bookmarkp-su-or-sudo-regexp "\\(/su:\\|/sudo:\\)"
  "*Regexp to recognize `su' or `sudo' Tramp bookmarks."
  :type 'regexp :group 'bookmarkp)

(defcustom bookmarkp-w3m-allow-multi-tabs t
  "*Non-nil means jump to W3m bookmarks in a new session."
  :type 'boolean :group 'bookmarkp)

(defcustom bookmarkp-show-end-of-region t
  "*Show end of region with `exchange-point-and-mark' when activating a region.
If nil show only beginning of region."
  :type 'boolean :group 'bookmarkp)

(defcustom bookmarkp-bookmark-name-length-max 70
  "*Maximum number of characters used to name a bookmark with region."
  :type 'integer :group 'bookmarkp)

(defcustom bookmarkp-bmenu-sort-function 'bookmarkp-visited-more-p
  "*Prefered function to sort bookmarks.
Possible values are:
`bookmarkp-visited-more-p' - sort by visit frequency
`bookmarkp-last-time-p' - sort by more recents visits
`bookmarkp-alpha-more-p' - sort alphabetically."
  :type '(choice (const :tag "None" nil) function) :group 'bookmarkp)

(defcustom bookmarkp-search-prompt "Pattern: "
  "*Prompt used for `bookmarkp-bmenu-search'."
  :type 'string :group 'bookmarkp)

(defcustom bookmarkp-search-delay 0.6
  "*Display when searching bookmarks is updated all `bookmarkp-search-delay' seconds."
  :type 'integer :group 'bookmarkp)

(defcustom bookmarkp-local-man-name-regexp "^NOM$"
  "*The translation of the uppercase word NAME in your language.
Used in `bookmark-set' to get the default bookmark name."
  :type 'string :group 'bookmarkp)

;;; Internal Variables --------------------------------------------------

(defvar bookmarkp-jump-display-function nil
  "Function used currently to display a bookmark.")

(defvar bookmarkp-latest-bookmark-alist ()
  "Content of `bookmark-alist' as last filtered.")

(defconst bookmarkp-non-file-filename "   - no file -"
  "Name to use for `filename' entry, for non-file bookmarks.")

(defvar bookmarkp-bookmark-marked-list nil
  "A list that contains all marked bookmarks.")

(defvar bookmarkp-bmenu-before-hide-unmarked-list nil
  "Store the list like it was before hiding unmarked bookmarks.")

(defvar bookmarkp-bmenu-before-hide-marked-list nil
  "Store the list like it was before hiding marked bookmarks.")

(defvar bookmarkp-bmenu-called-from-inside-flag nil
  "Signal `bookmark-bmenu-list' is called from bmenu-list buffer.")

(defvar bookmarkp-bmenu-reverse-sort-p nil
  "Reverse order of sorting.")

(defvar bookmarkp-search-pattern ""
  "Store keyboard input for incremental search.")

(defvar bookmarkp-search-timer nil
  "Timer used for searching")

;; REPLACES ORIGINAL DOC STRING in `bookmark.el'.
;;
;; Doc string reflects Bookmark+ enhancements.
;;

;; Apparently, we need to add this `defvar' stump, in order to get the documentation
;; to show up using `C-h v'.
(defvar bookmark-alist)

(put 'bookmark-alist 'variable-documentation
     "Association list of bookmarks and their records.
Bookmark functions update the value automatically.
You probably do not want to change the value yourself.

The value is an alist with entries of the form
 (BOOKMARK-NAME . PARAM-ALIST)
or the deprecated form (BOOKMARK-NAME PARAM-ALIST).

 BOOKMARK-NAME is the name you provided for the bookmark.
 PARAM-ALIST is an alist of bookmark information.  The order of the
  entries in PARAM-ALIST is not important.  The possible entries are
  described below.  A nil value means the entry is not used.

Bookmarks created using vanilla Emacs (`bookmark.el'):

 (filename . FILENAME)
 (position . POS)
 (front-context-string . STR-AFTER-POS)
 (rear-context-string  . STR-BEFORE-POS)
 (annotation . ANNOTATION)
 (handler . HANDLER)

 FILENAME names the bookmarked file.
 POS is the bookmarked buffer position (position in the file).
 STR-AFTER-POS is buffer text that immediately follows POS.
 STR-BEFORE-POS is buffer text that immediately precedes POS.
 ANNOTATION is a string that you can provide to identify the bookmark.
  See options `bookmark-use-annotations' and
  `bookmark-automatically-show-annotations'.
 HANDLER is a function that provides the bookmark-jump behavior
  for a specific kind of bookmark.  This is the case for Info
  bookmarks, for instance (starting with Emacs 23).

Bookmarks created using Bookmark+ are the same as for vanilla Emacs,
except for the following differences.

1. If no file is associated with the bookmark, then FILENAME is nil.

2. The following additional entries are used.  Their values are
non-nil when a region is bookmarked; they are nil otherwise.  When a
region is bookmarked, POS represents the region start position.

 (buffer-name . BUFFER-NAME)
 (end-position . END-POS)
 (front-context-region-string . STR-BEFORE-END-POS)
 (rear-context-region-string . STR-AFTER-END-POS))

 BUFFER-NAME is the name of a bookmarked buffer, which might not be
  associated with any file (see #1).
 END-POS is the region end position.
 STR-BEFORE-END-POS is buffer text that precedes END-POS.
 STR-AFTER-END-POS is buffer text that follows END-POS.

 NOTE: The relative locations of `front-context-region-string' and
 `rear-context-region-string' are reversed from those of
 `front-context-string' and `rear-context-string'.  For example,
 `front-context-string' is the text that *follows* `position', but
 `front-context-region-string' that *precedes* `end-position'.

3. The following additional entries are used for a Gnus bookmark.

 (group . GNUS-GROUP-NAME)
 (article . GNUS-ARTICLE-NUMBER)
 (message-id . GNUS-MESSAGE-ID)

 GNUS-GROUP-NAME is the name of a Gnus group.
 GNUS-ARTICLE-NUMBER is the number of a Gnus article.
 GNUS-MESSAGE-ID is the identifier of a Gnus message.

4. For a W3m bookmark, FILENAME is a W3m URL.")


;;; Core Replacements (`bookmark-*' except `bookmark-bmenu-*') -------

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Handles also regions and non-file buffers.
;;
(defun bookmark-make-record-default (&optional point-only)
  "Return the record describing the location of a new bookmark.
The cursor must be at the position where the bookmark is to be set.
If POINT-ONLY is non-nil, return only the subset of the record that
pertains to the location within the buffer."
  (let* ((isregion (and transient-mark-mode mark-active (not (eq (mark) (point)))))
         (isdired  (car (rassq (current-buffer) dired-buffers)))
         (beg      (if isregion (region-beginning) (point)))
         (end      (if isregion (region-end) (point)))
         (buf      (buffer-name))
         (fcs      (if isregion
                       (bookmarkp-region-record-front-context-string beg end)
                       (bookmarkp-record-front-context-string beg)))
         (rcs      (if isregion
                       (bookmarkp-region-record-rear-context-string beg)
                       (bookmarkp-record-rear-context-string beg)))
         (fcrs     (when isregion (bookmarkp-record-front-context-region-string beg end)))
         (ecrs     (when isregion (bookmarkp-record-end-context-region-string end)))
         (ctime    (bookmarkp-current-sec-time)))
    `(,@(unless point-only `((filename . ,(cond ((buffer-file-name (current-buffer))
                                                 (bookmark-buffer-file-name))
                                                (isdired)
                                                (t  bookmarkp-non-file-filename)))))
        (buffer-name . ,buf)
        (front-context-string . ,fcs)
        (rear-context-string . ,rcs)
        (front-context-region-string . ,fcrs)
        (rear-context-region-string . ,ecrs)
        (visits . 0)
        (time . ,ctime)
        (position . ,beg)
        (end-position . ,end))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Use `bookmark-make-record'.
;; Use special default prompts for active region, W3m, and Gnus.
;;
(defun bookmark-set (&optional name parg)
  "Set a bookmark named NAME.
If the region is active (`transient-mark-mode') and nonempty, then
record the region limits in the bookmark.

If NAME is nil, then prompt for the bookmark name.  The default name
for prompting is as follows (in order of priority):

 * If the region is active and nonempty, then the buffer name followed
   by \": \" and the region prefix (up to
   `bookmarkp-bookmark-name-length-max' chars).

 * If in W3m mode, then the current W3m title.

 * If in Gnus Summary mode, then the Gnus summary article header.

 * Otherwise, the current buffer name.

While entering a bookmark name at the prompt:

 * You can use `C-w' to yank words from the buffer to the minibuffer.
   Repeating `C-w' yanks successive words.

 * You can use `C-u' to insert the name of the last bookmark used in
   the buffer.  This can be useful as an aid to track your progress
   through a large file.  (If no bookmark has yet been used, then
   `C-u' inserts the name of the visited file.)

With a prefix argument, do not overwrite a bookmark that has the same
name as NAME, if such a bookmark already exists.  Instead, push the
new bookmark onto the bookmark alist.

The most recently set bookmark named NAME is thus the one in effect at
any given time, but any others named NAME are still available, should
you decide to delete the most recent one.

Use `\\[bookmark-delete]' to remove bookmarks (you give it a name, and it removes
only the first instance of a bookmark with that name from the list of
bookmarks.)"
  (interactive (list nil current-prefix-arg))
  (bookmark-maybe-load-default-file)
  (setq bookmark-current-buffer  (current-buffer))
  (save-excursion (skip-chars-forward " ") (setq bookmark-yank-point (point)))
  (let* ((record  (bookmark-make-record))
         (regionp (and transient-mark-mode mark-active (not (eq (mark) (point)))))
         (regname (concat (buffer-name) ": "
                          (buffer-substring
                           (if regionp (region-beginning) (point))
                           (if regionp
                               (region-end)
                               (save-excursion (end-of-line) (point))))))
         (defname (replace-regexp-in-string
                   "\n" " "
                   (cond (regionp
                          (save-excursion (goto-char (region-beginning))
                                          (skip-chars-forward " ") (setq bookmark-yank-point (point)))
                          (substring regname 0
                                     (min bookmarkp-bookmark-name-length-max
                                          (length regname))))
                         ((eq major-mode 'w3m-mode)
                          w3m-current-title)
                         ((eq major-mode 'gnus-summary-mode)
                          (elt (gnus-summary-article-header) 1))
                         ((or (eq major-mode 'woman-mode) (eq major-mode 'Man-mode))
                          (save-excursion
                            (goto-char (point-min))
                            (when (or (re-search-forward bookmarkp-local-man-name-regexp nil t)
                                      (re-search-forward "^NAME$" nil t))
                            (forward-line 1) (skip-chars-forward " ")
                            (buffer-substring-no-properties (point) (line-end-position)))))
                         (t (car record)))))
         (doc-cmd "`\\<minibuffer-local-map>\\[next-history-element]' \ for default")
         (bname   (or name
                      (read-from-minibuffer
                       (format "Set bookmark (%s): "
                               (if (> emacs-major-version 21)
                                   (substitute-command-keys doc-cmd)
                                   defname))
                       nil
                       (let ((map  (copy-keymap minibuffer-local-map)))
                         (define-key map "\C-w" 'bookmark-yank-word)
                         (define-key map "\C-u" 'bookmark-insert-current-bookmark)
                         map)
                       nil nil defname))))
    (when (string-equal bname "") (setq bname  defname))
    (bookmark-store bname (cdr record) parg)
    ;; Ask for an annotation buffer for this bookmark
    (when bookmark-use-annotations
      (bookmark-edit-annotation bname))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Prevent adding a newline in a bookmark name when yanking.
;; 
;;;###autoload
(defun bookmark-yank-word ()
  "Yank the word at point in `bookmark-current-buffer'.
Repeat to yank subsequent words from the buffer, appending them.
Newline characters are stripped out."
  (interactive)
  (let ((string  (with-current-buffer bookmark-current-buffer
                   (goto-char bookmark-yank-point)
                   (buffer-substring-no-properties
                    (point)
                    (progn (forward-word 1) (setq bookmark-yank-point (point)))))))
    (setq string  (replace-regexp-in-string "\n" "" string))
    (insert string)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Add marked bookmark to `bookmarkp-bookmark-marked-list',
;; Don't call a second time `bookmark-bmenu-check-position'.
;;
;;;###autoload
(defun bookmark-bmenu-mark ()
  "Mark bookmark on this line to be displayed by \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-select]."
  (interactive)
  (beginning-of-line)
  (when (bookmark-bmenu-check-position)
    (let ((inhibit-read-only t)
          (bmk (bookmark-bmenu-bookmark)))
      (push bmk bookmarkp-bookmark-marked-list)
      (delete-char 1)
      (insert ?>)
      (forward-line 1))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Remove marked bookmark of `bookmarkp-bookmark-marked-list'.
;; Don't call a second time `bookmark-bmenu-check-position'.
;;
;;;###autoload
(defun bookmark-bmenu-unmark (&optional backup)
  "Cancel all requested operations on bookmark on this line and move down.
Optional BACKUP means move up."
  (interactive "P")
  (beginning-of-line)
  (when (bookmark-bmenu-check-position)
    (let ((inhibit-read-only t)
          (bmk (bookmark-bmenu-bookmark)))
      (delete-char 1)
      (insert " ")
      (setq bookmarkp-bookmark-marked-list
            (remove bmk bookmarkp-bookmark-marked-list)))
    (forward-line (if backup -1 1))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Get bookmark name from `bookmarkp-bookmark-name' property of bookmark.
;; 
(defun bookmark-bmenu-bookmark ()
  "Return the name of the bookmark on this line."
  (when (bookmark-bmenu-check-position)
    (save-excursion
      (forward-line 0) (forward-char 3)
      (get-text-property (point) 'bookmarkp-bookmark-name))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Return t instead of returning `bookmark-alist'
;; to increase performances of display.
;;
(defun bookmark-bmenu-check-position ()
  "Ensure point is not outside of bookmark list.
e.g in the first two lines or on end line.
Returns non-nil if on a line with a bookmark and
`bookmark-alist' is not empty."
  (unless (zerop (length bookmark-alist)) ; Be sure we have bookmarks
    (cond ((< (count-lines (point-min) (point)) 2)
           (goto-char (point-min))
           (forward-line 2) t)
          ((and (bolp) (eobp))
           (beginning-of-line 0) t)
          (t
           t))))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Fix bug in emacs-23.1-r1 with `called-interactively-p'
;; Call *-jump-via that is not interactive to fix that.
;;
(defun bookmark-bmenu-this-window ()
  "Select this line's bookmark in this window."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
  (when (bookmark-bmenu-check-position)
    (bookmark--jump-via bookmark 'switch-to-buffer))))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Save DISPLAY-FUNCTION to `bookmarkp-jump-display-function' before calling
;; `bookmark-handle-bookmark'.
;;
(defun bookmark--jump-via (bookmark display-function)
  "Helper function for `bookmark-jump(-other-window)'.
BOOKMARK is a bookmark name or a bookmark record.
DISPLAY-FUNCTION is the function that displays the bookmark."
  (bookmarkp-update-time-and-increment-visits bookmark)
  (setq bookmarkp-jump-display-function  display-function)
  (bookmark-handle-bookmark bookmark)
  (let ((win  (get-buffer-window (current-buffer) 0)))
    (when win (set-window-point win (point))))
  ;; VANILLA EMACS FIXME: we used to only run `bookmark-after-jump-hook' in
  ;; `bookmark-jump' itself, but in none of the other commands.
  (run-hooks 'bookmark-after-jump-hook)
  (when bookmark-automatically-show-annotations
    (bookmark-show-annotation bookmark)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Avoid using `nconc'
;;
(defun bookmark-prop-set (bookmark prop val)
  "Set the property PROP of BOOKMARK to VAL."
  (let ((bmk   (bookmark-get-bookmark bookmark))
        (cell  (assq prop (bookmark-get-bookmark-record bookmark))))
    (if cell
        (setcdr cell val)
        (if (consp (car (cadr bmk)))      ; Old format: ("name" ((filename . "f")...))
            (setcdr bmk (list (cons (cons prop val) (cadr bmk))))
            (setcdr bmk (cons (cons prop val) (cdr bmk))))))) ; New: ("name" (filename . "f")...)


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arg USE-REGION-P.
;; 2. Add note about Icicles `S-delete' to doc string.
;;
;;;###autoload
(defun bookmark-jump (bookmark-name &optional use-region-p)
  "Jump to the bookmark named BOOKMARK-NAME.
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See function `bookmark-load' for more about this.

If the file pointed to by BOOKMARK-NAME no longer exists, you are
asked if you wish to give the bookmark a new location.  If so,
`bookmark-jump' jumps to the new location and saves it.

If the bookmark represents a region, then the region is activated if
`bookmarkp-use-region-flag' is not-nil or it is nil and you use a
prefix argument.  A prefix arg temporarily flips the value of
`bookmarkp-use-region-flag'.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Jump to bookmark" bookmark-current-bookmark)
                     current-prefix-arg))
  (unless bookmark-name (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark-name)
  (let ((bookmarkp-use-region-flag  (if use-region-p
                                        (not bookmarkp-use-region-flag)
                                        bookmarkp-use-region-flag)))
    (bookmark--jump-via bookmark-name 'switch-to-buffer)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Added optional arg USE-REGION-P.
;;
;;;###autoload
(defun bookmark-jump-other-window (bookmark-name &optional use-region-p)
  "Jump to the bookmark named BOOKMARK-NAME, in another window.
See `bookmark-jump'."
  (interactive (list (bookmark-completing-read
                      "Jump to bookmark (in another window)"
                      bookmark-current-bookmark)
                     current-prefix-arg))
  (unless bookmark-name (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark-name)
  (let ((bookmarkp-use-region-flag  (if use-region-p
                                        (not bookmarkp-use-region-flag)
                                        bookmarkp-use-region-flag)))
    (bookmark--jump-via bookmark-name 'switch-to-buffer-other-window)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Support regions and buffer names.
;;
(defun bookmark-default-handler (bookmark)
  "Default handler to jump to the location of BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
If BOOKMARK records a nonempty region, and `bookmarkp-use-region-flag'
 is non-nil, then activate the region.
Return nil or signal `file-error'."
  (let* ((bmk      (bookmark-get-bookmark bookmark)) ; Get bookmark object once and for all.
         (file     (bookmark-get-filename bmk))
         (buf      (bookmark-prop-get bmk 'buffer))
         (bufname  (bookmarkp-get-buffer-name bmk))
         (pos      (bookmark-get-position bmk))
         (end-pos  (bookmarkp-get-end-position bmk)))
    (if (not (and bookmarkp-use-region-flag end-pos (/= pos end-pos)))
        ;; Single-position bookmark (no region).  Go to it.
        (bookmarkp-goto-position file buf bufname pos
                                 (bookmark-get-front-context-string bmk)
                                 (bookmark-get-rear-context-string bmk))
        ;; Bookmark with a region.  Go to it and activate the region.
        (if (and file (file-readable-p file) (not (buffer-live-p buf)))
            (with-current-buffer (find-file-noselect file) (setq buf  (buffer-name)))
            ;; No file found.  If no buffer either, then signal that file doesn't exist.
            (unless (or (and buf (get-buffer buf))
                        (and bufname (get-buffer bufname) (not (string= buf bufname))))
              (signal 'file-error `("Jumping to bookmark" "No such file or directory"
                                                          (bookmark-get-filename bmk)))))
        (set-buffer (or buf bufname))
        (save-current-buffer (funcall bookmarkp-jump-display-function (current-buffer)))
        (raise-frame)
        (goto-char (min pos (point-max)))
        (when (> pos (point-max)) (error "Bookmark position is beyond buffer end"))
        ;; Activate region.  Relocate it if it moved.  Save relocated bookmark if confirm.
        (funcall bookmarkp-handle-region-function bmk))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Location returned can be a buffer name, instead of a file name.
;;
(defun bookmark-location (bookmark)
  "Return the name of the file or buffer associated with BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-maybe-load-default-file)
  (or (bookmark-get-filename bookmark)
      (bookmarkp-get-buffer-name bookmark)
      (bookmark-prop-get bookmark 'buffer)
      (error "Bookmark has no file or buffer name: %S" bookmark)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Added note about `S-delete' to doc string.
;; Added BATCH arg.
;;
;;;###autoload
(defun bookmark-rename (old &optional new batch)
  "Change bookmark's name from OLD to NEW.
Interactively:
 If called from the keyboard, then prompt for OLD.
 If called from the menubar, select OLD from a menu.
If NEW is nil, then prompt for its string value.

If BATCH is non-nil, then do not rebuild the menu list.

While the user enters the new name, repeated `C-w' inserts consecutive
words from the buffer into the new bookmark name.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Old bookmark name")))
  (bookmark-maybe-historicize-string old)
  (bookmark-maybe-load-default-file)
  (save-excursion (skip-chars-forward " ") (setq bookmark-yank-point (point)))
  (setq bookmark-current-buffer (current-buffer))
  (let ((newname  (or new  (read-from-minibuffer
                            "New name: " nil
                            (let ((now-map  (copy-keymap minibuffer-local-map)))
                              (define-key now-map  "\C-w" 'bookmark-yank-word)
                              now-map)
                            nil 'bookmark-history))))
    (bookmark-set-name old newname)
    (setq bookmark-current-bookmark  newname)
    (unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
    (bookmarkp-maybe-save-bookmark) newname))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Add note about `S-delete' to doc string.
;; Change arg name: BOOKMARK -> BOOKMARK-NAME.
;;
(or (fboundp 'old-bookmark-insert)
    (fset 'old-bookmark-insert (symbol-function 'bookmark-insert)))

;;;###autoload
(defun bookmark-insert (bookmark-name)
  "Insert the text of a bookmarked file.
BOOKMARK-NAME is the name of the bookmark.
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See function `bookmark-load' for more about this.

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate."
  (interactive (list (bookmark-completing-read "Insert bookmark contents")))
  (old-bookmark-insert bookmark-name))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Add note about `S-delete' to doc string.
;; Change arg name: BOOKMARK -> BOOKMARK-NAME.
;; Increment `bookmark-alist-modification-count' even when using `batch' arg.
;;
;;;###autoload
(defun bookmark-delete (bookmark-name &optional batch)
  "Delete the bookmark named BOOKMARK-NAME from the bookmark list.
Removes only the first instance of a bookmark with that name.
If there are other bookmarks with the same name, they are not deleted.
Defaults to the \"current\" bookmark (that is, the one most recently
used in this file), if it exists.  Optional second arg BATCH means do
not update the bookmark list buffer (probably because we were called
from there).

If you use Icicles, then you can use `S-delete' during completion of a
bookmark name to delete the bookmark named by the current completion
candidate.  In this way, you can delete multiple bookmarks."
  (interactive
   (list (bookmark-completing-read "Delete bookmark"
				   bookmark-current-bookmark)))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let ((will-go (bookmark-get-bookmark bookmark-name 'noerror)))
    (setq bookmark-alist (delete will-go bookmark-alist))
    ;; Added by db, nil bookmark-current-bookmark if the last
    ;; occurrence has been deleted
    (setq bookmarkp-latest-bookmark-alist (delete will-go bookmarkp-latest-bookmark-alist))
    (or (bookmark-get-bookmark bookmark-current-bookmark 'noerror)
        (setq bookmark-current-bookmark nil))
    ;; Don't rebuild the list when using `batch' arg
    (unless batch
      (bookmark-bmenu-surreptitiously-rebuild-list))
    (bookmarkp-maybe-save-bookmark)))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Improved performances when saving `bookmark-alist' by inserting
;; little chunk of code one by one instead of letting `pp' parse the whole
;; `bookmark-alist' in one time, what is very long.
;; Remove text properties in bookmark title if some.
;;
(defun bookmark-write-file (file)
  "Write `bookmark-alist' to `bookmark-default-file'."
  (bookmark-maybe-message "Saving bookmarks to file `%s'..." file)
  (with-current-buffer (get-buffer-create " *Bookmarks*")
    (goto-char (point-min))
    (delete-region (point-min) (point-max))
    (let ((print-length  nil)
          (print-level   nil))
      (bookmark-insert-file-format-version-stamp)
      (progn (insert "(")
             (dolist (i  bookmark-alist)
               (let* ((str       (car i))
                      (len-str   (length str))
                      (fname     (cdr (assoc 'filename i)))
                      (len-fname (length fname)))
                 (set-text-properties 0 len-str nil str)
                 (when fname (set-text-properties 0 len-fname nil fname))
                 (setcar i str) (setcdr (assoc 'filename i) fname)
                 (pp i (current-buffer))))
             (insert ")"))
      (let ((version-control  (cond ((null bookmark-version-control) nil)
                                    ((eq 'never bookmark-version-control) 'never)
                                    ((eq 'nospecial bookmark-version-control)
                                     version-control)
                                    (t t))))
        (condition-case nil
            (write-region (point-min) (point-max) file)
          (file-error (message "Cannot write file `%s'" file)))
        (kill-buffer (current-buffer))
        (bookmark-maybe-message "Saving bookmarks to file `%s'...done" file)))))


;;; Menu List Replacements (`bookmark-bmenu-*') ----------------------


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Rebuild `bookmark-alist' using the last filtered alist in use.
;; 2. Update the menu-list title.
;;
(defun bookmark-bmenu-surreptitiously-rebuild-list ()
  "Rebuild the Bookmark List if it exists.
Don't affect the buffer ring order."
  (when (get-buffer "*Bookmark List*")
    (save-excursion
      (save-window-excursion
        (let ((bookmark-alist  bookmarkp-latest-bookmark-alist)
              (title           (with-current-buffer "*Bookmark List*"
                                 (goto-char (point-min))
                                 (buffer-substring (line-beginning-position)
                                                   (line-end-position)))))
          (bookmark-bmenu-list title 'filteredp))))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Added optional arguments TITLE and FILTER-ON.
;; 2. Handles also region bookmarks and buffer (non-file) bookmarks.
;;
;;;###autoload
(defun bookmark-bmenu-list (&optional title filteredp)
  "Display a list of existing bookmarks, in buffer `*Bookmark List*'.
The following faces are used for the list entries.
Use `customize-face' if you want to change the appearance.

  `bookmarkp-local-directory', `bookmarkp-local-file-without-region',
  `bookmarkp-local-file-with-region', `bookmarkp-gnus',
  `bookmarkp-info', `bookmarkp-non-file', `bookmarkp-remote-file',
  `bookmarkp-su-or-sudo', `bookmarkp-w3m'.

The optional args are for non-interactive use.
TITLE is a string to be used as the title.
 The default title is `% Bookmark+'.
The leftmost column of a bookmark entry shows `D' if the bookmark is
 flagged for deletion, or `>' if it is marked for displaying.

Non-nil FILTEREDP indicates that `bookmark-alist' has been filtered
\(e.g gnus, w3m, info, files, or regions).  In that case,
`bookmarkp-latest-bookmark-alist' is not reset to `bookmark-alist'."
  (interactive)
  (bookmark-maybe-load-default-file)
  (unless bookmarkp-bmenu-called-from-inside-flag (setq bookmarkp-bookmark-marked-list nil))
  (unless filteredp (setq bookmarkp-latest-bookmark-alist bookmark-alist))
  (if (interactive-p)
      (switch-to-buffer (get-buffer-create "*Bookmark List*"))
      (set-buffer (get-buffer-create "*Bookmark List*")))
  (let* ((inhibit-read-only  t)
         (alternate-title    (if title title "% Bookmark+"))
         (len-alt-title      (- (length alternate-title) 2)))
    (erase-buffer)
    (insert (format "%s\n- %s\n" alternate-title (make-string len-alt-title ?-)))
    (add-text-properties (point-min) (point) '(font-lock-face bookmark-menu-heading))
    (mapc (lambda (full-record)
            ;; If a bookmark has an annotation, prepend a "*" in the list of bookmarks.
            (let ((name        (bookmark-name-from-full-record full-record))
                  (annotation  (bookmark-get-annotation full-record))
                  (marked      (bookmarkp-bookmark-marked-p full-record))
                  (start       (+ 2 (point)))
                  end)
              (insert (cond ((and annotation (not (string-equal annotation "")) marked) ">*")
                            ((and annotation (not (string-equal annotation "")))  " *")
                            (marked "> ")
                            (t "  "))
                      name)
              (setq end (point))
              (bookmarkp-bmenu-propertize-item name start end)
              (insert "\n")))
          (bookmarkp-bmenu-maybe-sort))
    (goto-char (point-min))
    (forward-line 2)
    (bookmark-bmenu-mode)
    (when bookmark-bmenu-toggle-filenames (bookmark-bmenu-toggle-filenames t))
    (when (fboundp 'fit-frame-if-one-window) (fit-frame-if-one-window))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Add text properties when hiding filenames.
;;
(defun bookmark-bmenu-hide-filenames (&optional force)
  "Hide filename visibility in bookmark-list buffer."
  (when (and (not force)  bookmark-bmenu-toggle-filenames)
    ;; nothing to hide if above is nil
    (save-excursion
      (save-window-excursion
        (goto-char (point-min))
        (forward-line 2)
        (setq bookmark-bmenu-hidden-bookmarks  (nreverse bookmark-bmenu-hidden-bookmarks))
        (setq bookmark-bmenu-bookmark-column 2)
        (save-excursion
          (let ((inhibit-read-only t))
            (while bookmark-bmenu-hidden-bookmarks
              (move-to-column bookmark-bmenu-bookmark-column t)
              (bookmark-kill-line)
              (let ((name  (car bookmark-bmenu-hidden-bookmarks))
                    (start (point))
                    end)  
                (insert name) (setq end (point))
                (bookmarkp-bmenu-propertize-item name start end))
              (setq bookmark-bmenu-hidden-bookmarks  (cdr bookmark-bmenu-hidden-bookmarks))
              (forward-line 1))))))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Use `pop-to-buffer', not `switch-to-buffer-other-window'.
;;
(defun bookmark-bmenu-other-window ()
  "Select this line's bookmark in other window, leaving bookmark menu visible."
  (interactive)
  (let ((bookmark  (bookmark-bmenu-bookmark)))
    (when (bookmark-bmenu-check-position)
      (let ((bookmark-automatically-show-annotations  t)) ;FIXME: needed?
        (bookmark--jump-via bookmark 'pop-to-buffer)))))


;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; 1. Use  `bookmark-bmenu-surreptitiously-rebuild-list', instead of using
;; `bookmark-bmenu-list', updating the modification count, and saving.
;; 2. Update `bookmarkp-latest-bookmark-alist' to reflect deletion.
;;
;;;###autoload
(defun bookmark-bmenu-execute-deletions (&optional markedp)
  "Delete bookmarks marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-delete] commands."
  (interactive "P")
  (if (or (not markedp) (yes-or-no-p "Delete bookmarks marked `>' (not `D') "))
      (let* ((o-point    (point))
             (which-mark (if markedp "^>" "^D"))
             (o-str      (unless (looking-at which-mark) (bookmark-bmenu-bookmark)))
             (count      0))
        (message "Deleting bookmarks...")
        (goto-char (point-min))
        (forward-line 2)
        (while (re-search-forward which-mark (point-max) t)
          (let ((bmk (bookmark-bmenu-bookmark))) 
            (bookmark-delete bmk 'batch) ; pass BATCH arg
            (setq bookmarkp-latest-bookmark-alist
                  (delete (assoc bmk bookmarkp-latest-bookmark-alist)
                          bookmarkp-latest-bookmark-alist))
            (setq count (1+ count))))
        (if (> count 0)
            (progn
              (setq bookmarkp-bmenu-called-from-inside-flag t)
              (bookmark-bmenu-surreptitiously-rebuild-list)
              (message "Deleting %s bookmarks...done" count))
            (message "Nothing to delete here"))
        (if o-str
            (bookmarkp-bmenu-goto-bookmark-named o-str)
            (goto-char o-point) (beginning-of-line)))
      (message "OK, nothing deleted")))

;; REPLACES ORIGINAL in `bookmark.el'.
;;
;; Don't call `bookmark-bmenu-list' (it was already called).
;;
;;;###autoload
(defun bookmark-bmenu-rename ()
  "Rename bookmark on current line.  Prompts for a new name."
  (interactive)
  (when (bookmark-bmenu-check-position)
    (let* ((bmk       (bookmark-bmenu-bookmark))
           (new-name  (bookmark-rename bmk)))
      (when (or (search-forward new-name (point-max) t)
                (search-backward new-name (point-min) t))
        (beginning-of-line)))))


;;; Bookmark+ Functions (`bookmarkp-*') ------------------------------

;; Don't require cl at runtime.
(defun bookmarkp-remove-if (pred xs)
  "A copy of list XS with no elements that satisfy predicate PRED."
  (loop for i in xs unless (funcall pred i) collect i into result
     finally return result))

(defun bookmarkp-remove-if-not (pred xs)
  "A copy of list XS with only elements that satisfy predicate PRED."
  (loop for i in xs when (funcall pred i) collect i into result
     finally return result))
  

(defun bookmarkp-maybe-save-bookmark ()
  "Increment save counter and maybe save `bookmark-alist'."
  (setq bookmark-alist-modification-count (1+ bookmark-alist-modification-count))
  (when (bookmark-time-to-save-p) (bookmark-save)))

(defun bookmarkp-edit-bookmark (bookmark-name)
  "Edit bookmark's name and file name, and maybe save them.
BOOKMARK-NAME is the current (old) name of the bookmark to be renamed."
  (let* ((bookmark-filename  (bookmark-get-filename bookmark-name))
         (new-name           (read-from-minibuffer "Name: " nil nil nil nil bookmark-name))
         (new-filename       (read-from-minibuffer "FileName: " nil nil nil nil
                                                   bookmark-filename)))
    (when (and (not (equal new-name "")) (not (equal new-filename ""))
               (y-or-n-p "Save changes?"))
      (bookmark-rename bookmark-name new-name 'batch)
      (bookmark-set-filename new-name new-filename)
      (bookmarkp-maybe-save-bookmark)
      (list new-name new-filename))))

(defun bookmarkp-increment-visits (bmk)
  "Increment visits entry of bmk.
If bmk have no visits entry, add one with value 0."
  (let ((cur-val (bookmark-prop-get bmk 'visits)))
    (if cur-val
        (bookmark-prop-set bmk 'visits (1+ cur-val))
        (bookmark-prop-set bmk 'visits 0))))


(defun bookmarkp-add-or-update-time (bmk)
  "Update time entry of bmk.
If bmk have no time entry, add one with current time."
  (let ((time (float-time)))
    (bookmark-prop-set bmk 'time time)))

(defun bookmarkp-update-time-and-increment-visits (bmk &optional batch)
  "Update time and increment visits entry of BMK.
Unless batch arg is non--nil update display and increment save counter."
  (bookmarkp-increment-visits bmk)
  (bookmarkp-add-or-update-time bmk)
  (unless batch
    (setq bookmarkp-bmenu-called-from-inside-flag t)
    (bookmark-bmenu-surreptitiously-rebuild-list))
  (bookmarkp-maybe-save-bookmark))


;;; Sorting bookmarks
(defun bookmarkp-sort-p-1 (s1 s2)
  "General predicate for sorting bookmarks.
Return non-nil if bookmark S1 was visited more often than S2.
Also: S1 < S2 if S1 was visited but S2 was not.
      S1 < S2 if S1 precedes S2 alphabetically and
      neither was visited or both were visited equally."
  (let* ((sym (case bookmarkp-bmenu-sort-function
                ('bookmarkp-visited-more-p   'visits)
                ('bookmarkp-last-time-more-p 'time)
                (t nil)))
         (v1  (when sym (cdr (assq sym s1))))
         (v2  (when sym (cdr (assq sym s2)))))
    (cond ((and v1 v2)
           (or (> v1 v2)
               (and (= v1 v2) (string-lessp (car s1) (car s2)))))
          (v1 t)   ; Only s1 visited
          (v2 nil) ; Only s2 visited
          (t (string-lessp (car s1) (car s2))))))


;; Predicate for sorting bookmarks with visits entry.
(defalias 'bookmarkp-visited-more-p 'bookmarkp-sort-p-1)

;; Predicate for sorting bookmarks with time entry.
(defalias 'bookmarkp-last-time-more-p 'bookmarkp-sort-p-1)

;; Predicate for sorting bookmarks alphabetically.
(defalias 'bookmarkp-alpha-more-p 'bookmarkp-sort-p-1)


;; Menu-List Functions (`bookmarkp-bmenu-*') -------------------------


(defun bookmarkp-bmenu-maybe-sort (&optional alist)
  "Sort or reverse-sort using `bookmarkp-bmenu-sort-function'.
        Sort LIST using `bookmarkp-bmenu-sort-function'.
        Reverse the result if `bookmarkp-reverse-sort-p' is non-nil.
        Do nothing if `bookmarkp-bmenu-sort-function' is nil."
  (let ((bmk-alist (or alist (copy-sequence bookmark-alist))))
    (when (and bmk-alist bookmarkp-bmenu-sort-function)
      (sort
       bmk-alist
       (if bookmarkp-bmenu-reverse-sort-p
           (lambda (a b)
             (not (funcall bookmarkp-bmenu-sort-function a b)))
           bookmarkp-bmenu-sort-function)))))


(defun bookmarkp-bmenu-sort-1 (method &optional batch)
  "Set `bookmarkp-bmenu-sort-function' to `method' and rebuild alist.
Try to follow position of last bookmark in menu-list."
  (with-current-buffer "*Bookmark List*"
    (let ((bmk (when (bookmark-bmenu-check-position) (bookmark-bmenu-bookmark)))
          (bookmarkp-bmenu-called-from-inside-flag t))
      (setq bookmarkp-bmenu-sort-function method)
      (case method
        ('bookmarkp-visited-more-p (if bookmarkp-bmenu-reverse-sort-p
                                       (message "Sorting by visit frequency [REVERSED]")
                                       (message "Sorting by visit frequency")))
        ('bookmarkp-last-time-more-p (if bookmarkp-bmenu-reverse-sort-p
                                         (message "Sorting by last time visited [REVERSED]")
                                         (message "Sorting by last time visited")))
        ('bookmarkp-alpha-more-p (if bookmarkp-bmenu-reverse-sort-p
                                     (message "Sorting alphabetically [REVERSED]")
                                     (message "Sorting alphabetically"))))
      (unless batch
        (bookmark-bmenu-surreptitiously-rebuild-list)
        (bookmarkp-bmenu-goto-bookmark-named bmk)))))

(defun bookmarkp-bmenu-goto-bookmark-named (name)
  "Go to the first bookmark whose name matches NAME."
  (goto-char (point-min))
  (bookmark-bmenu-check-position)
  (while (not (equal name (bookmark-bmenu-bookmark)))
    (forward-line 1))
  (forward-line 0))

;;;###autoload
(defun bookmarkp-bmenu-sort-by-visit-frequency (&optional reversep)
  (interactive "P")
  (let ((bookmarkp-bmenu-reverse-sort-p reversep))
    (bookmarkp-bmenu-sort-1 'bookmarkp-visited-more-p)))

;;;###autoload
(defun bookmarkp-bmenu-sort-by-last-time-visited (&optional reversep)
  (interactive "P")
  (let ((bookmarkp-bmenu-reverse-sort-p reversep))
    (bookmarkp-bmenu-sort-1 'bookmarkp-last-time-more-p)))

;;;###autoload
(defun bookmarkp-bmenu-sort-alphabetically (&optional reversep)
  (interactive "P")
  (let ((bookmarkp-bmenu-reverse-sort-p reversep))
    (bookmarkp-bmenu-sort-1 'bookmarkp-alpha-more-p)))

;;; Searching in bookmarks
;;
;;  Narrow down `bookmark-alist' with only bookmarks matching regexp.
;;  Display is updated at each time a character is entered in minibuffer.
;;
(defun bookmarkp-read-search-input ()
  "Read each keyboard input and add it to `bookmarkp-search-pattern'."
  (setq bookmarkp-search-pattern "")    ; Always reset pattern to empty string
  (let ((tmp-list ())
        (prompt   (propertize bookmarkp-search-prompt 'face '((:foreground "cyan"))))
        char)
    (catch 'break
      (while 1
        (catch 'continue
          (condition-case nil
              (setq char (read-char (concat prompt bookmarkp-search-pattern)))
            (error (throw 'break nil)))
          (case char
            ((or ?\e ?\r) (throw 'break nil))    ; RET or ESC break and exit code.
            (?\d (pop tmp-list)         ; Delete last char of `bookmarkp-search-pattern' with DEL
                 (setq bookmarkp-search-pattern (mapconcat 'identity (reverse tmp-list) ""))
                 (throw 'continue nil))
            (t
             (push (text-char-description char) tmp-list)
             (setq bookmarkp-search-pattern (mapconcat 'identity (reverse tmp-list) ""))
             (throw 'continue nil))))))))


(defun bookmarkp-filtered-alist-by-regexp-only (regexp alist)
  "Return a filtered ALIST with (only) bookmarks matching REGEXP."
  (bookmarkp-remove-if-not #'(lambda (x) (string-match regexp (car x))) alist))

(defun bookmarkp-bmenu-filter-alist-by-regexp (regexp alist)
  "Display (only) bookmarks of ALIST matching REGEXP."
  (let ((bookmark-alist (bookmarkp-filtered-alist-by-regexp-only regexp alist))
        (bookmarkp-bmenu-called-from-inside-flag t)) ; Dont remove marks if some.
    (setq bookmarkp-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark+ Filtered by regexp" 'filteredp)))


;;;###autoload
(defun bookmarkp-bmenu-search (&optional all)
  "Incremental search of bookmarks matching `bookmarkp-search-pattern'.
We make search in the current list displayed i.e `bookmarkp-latest-bookmark-alist'.
If a prefix arg is given search in the whole `bookmark-alist'."
  (interactive "P")
  (lexical-let ((bmk-list (if all bookmark-alist bookmarkp-latest-bookmark-alist)))
    (unwind-protect
         (progn
           (setq bookmarkp-search-timer
                 (run-with-idle-timer 0 bookmarkp-search-delay
                                      #'(lambda ()
                                          (bookmarkp-bmenu-filter-alist-by-regexp bookmarkp-search-pattern bmk-list))))
           (bookmarkp-read-search-input))
      (bookmarkp-bmenu-cancel-search))))

(defun bookmarkp-bmenu-cancel-search ()
  "Cancel timer used for searching in bookmarks."
  (cancel-timer bookmarkp-search-timer)
  (setq bookmarkp-search-timer nil))


;;;###autoload
(defun bookmarkp-bmenu-edit-bookmark ()
  "Edit the bookmark under the cursor."
  (interactive)
  (when (bookmark-bmenu-check-position)
    (let* ((bmk-name  (bookmark-bmenu-bookmark))
           (new-data  (bookmarkp-edit-bookmark bmk-name))
           (new-name  (car new-data)))
      (if (not new-data)
          (message "No changes made")
          (bookmark-bmenu-surreptitiously-rebuild-list)
          (goto-char (point-min))
          (while (not (equal new-name (bookmark-bmenu-bookmark)))
            (forward-line 1))
          (forward-line 0)
          (bookmark-bmenu-check-position)))))


;;;###autoload
(defun bookmarkp-bmenu-delete-bookmark ()
  "Delete bookmark at point in Bookmark Menu list."
  (interactive)
  (when (equal (buffer-name (current-buffer)) "*Bookmark List*")
    (let ((bmk (bookmark-bmenu-bookmark))
          (pos (point)))
      (if (y-or-n-p "Delete this bookmark? ")
          (progn (bookmark-delete bmk) (goto-char pos))
          (message "Aborting bookmark deletion")))))


(defun bookmarkp-bmenu-propertize-item (bookmark-name start end)
  "Add text properties to BOOKMARK-NAME, from START to END."
  (let* ((isfile        (bookmark-get-filename bookmark-name))
         (isremote      (and isfile
                             (if (fboundp 'file-remote-p)
                                 (file-remote-p isfile)
                                 (if (fboundp 'ffap-file-remote-p)
                                     (ffap-file-remote-p isfile)))))
         (istramp       (and isfile (boundp 'tramp-file-name-regexp)
                             (save-match-data
                               (string-match tramp-file-name-regexp isfile))))
         (isw3m         (bookmarkp-w3m-bookmark-p bookmark-name))
         (issu          (and istramp (string-match bookmarkp-su-or-sudo-regexp
                                                   isfile)))
         (isregion      (bookmarkp-region-bookmark-p bookmark-name))
         (isannotation  (bookmark-get-annotation bookmark-name))
         (ishandler     (bookmark-get-handler bookmark-name))
         (isgnus        (bookmarkp-gnus-bookmark-p bookmark-name))
         (isbuf         (bookmarkp-get-buffer-name bookmark-name)))
    (put-text-property start end 'bookmarkp-bookmark-name bookmark-name)
    (add-text-properties
     start  end
     (cond ((or (eq ishandler 'Info-bookmark-jump) (string= isbuf "*info*")) ; Info
            '(mouse-face highlight follow-link t face bookmarkp-info
              help-echo "mouse-2: Go to this Info buffer"))
           (isgnus               ; Gnus
            '(mouse-face highlight follow-link t face bookmarkp-gnus
              help-echo "mouse-2: Go to this Gnus buffer"))
           (isw3m                ; W3m
            `(mouse-face highlight follow-link t face bookmarkp-w3m
                         help-echo (format "mouse-2 Goto URL: %s",isfile)))
           ((or (eq ishandler 'bookmarkp-jump-woman) ; Woman pages
                (eq ishandler 'bookmarkp-jump-man))  ; Man pages
            `(mouse-face highlight follow-link t face bookmarkp-woman
                         help-echo (format "mouse-2 Goto URL: %s",isfile)))
           ((and issu (not (bookmarkp-root-or-sudo-logged-p))) ; Root/sudo not logged
            `(mouse-face highlight follow-link t face bookmarkp-su-or-sudo
                         help-echo (format "mouse-2 Goto file: %s",isfile)))
           ((and isremote (not issu)) ; Remote file (ssh, ftp)
            `(mouse-face highlight follow-link t face bookmarkp-remote-file
                         help-echo (format "mouse-2 Goto remote file: %s",isfile)))
           ((and isfile (file-directory-p isfile)) ; Local directory
            `(mouse-face highlight follow-link t face bookmarkp-local-directory
                         help-echo (format "mouse-2 Goto dired: %s",isfile)))
           ((and isfile (file-exists-p isfile) isregion) ; Local file with region
            `(mouse-face highlight follow-link t face
                         bookmarkp-local-file-with-region
                         help-echo (format "mouse-2 Find region in file: %s",isfile)))
           ((and isfile (file-exists-p isfile)) ; Local file without region
            `(mouse-face highlight follow-link t face
                         bookmarkp-local-file-without-region
                         help-echo (format "mouse-2 Goto file: %s",isfile)))
           ((and isbuf (if isfile (not (file-exists-p isfile)) (not isfile))) ; No filename
            `(mouse-face highlight follow-link t face bookmarkp-non-file
                         help-echo (format "mouse-2 Goto buffer: %s",isbuf)))))))


;;;###autoload
(defun bookmarkp-bmenu-quit ()
  "Reset the marked bookmark lists and quit."
  (interactive)
  (setq bookmarkp-bookmark-marked-list nil)
  (setq bookmarkp-bmenu-before-hide-marked-list nil)
  (setq bookmarkp-bmenu-before-hide-unmarked-list nil)
  (quit-window))


;;; Filters *-bmenu-* commands

;;;###autoload
(defun bookmarkp-bmenu-list-only-file-bookmarks (arg)
  "Display a list of file and directory bookmarks (only).
With a prefix argument, do not include remote files or directories."
  (interactive "P")
  (let ((bookmark-alist  (bookmarkp-file-alist-only arg))
        (bookmarkp-bmenu-called-from-inside-flag t))
    (setq bookmarkp-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark+ Files&Directories" 'filteredp)))
         

;;;###autoload
(defun bookmarkp-bmenu-list-only-non-file-bookmarks ()
  "Display (only) the non-file bookmarks."
  (interactive)
  (let ((bookmark-alist (bookmarkp-non-file-alist-only))
        (bookmarkp-bmenu-called-from-inside-flag t))
    (setq bookmarkp-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark+ Non--Files" 'filteredp)))


;;;###autoload
(defun bookmarkp-bmenu-list-only-info-bookmarks ()
  "Display (only) the Info bookmarks."
  (interactive)
  (let ((bookmark-alist  (bookmarkp-info-alist-only))
        (bookmarkp-bmenu-called-from-inside-flag t))
    (setq bookmarkp-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark+ Info" 'filteredp)))


;;;###autoload
(defun bookmarkp-bmenu-list-only-w3m-bookmarks ()
  "Display (only) the w3m bookmarks."
  (interactive)
  (let ((bookmark-alist  (bookmarkp-w3m-alist-only))
        (bookmarkp-bmenu-called-from-inside-flag t))
    (setq bookmarkp-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark+ W3m" 'filteredp)))


;;;###autoload
(defun bookmarkp-bmenu-list-only-gnus-bookmarks ()
  "Display (only) the Gnus bookmarks."
  (interactive)
  (let ((bookmark-alist  (bookmarkp-gnus-alist-only))
        (bookmarkp-bmenu-called-from-inside-flag t))
    (setq bookmarkp-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark+ Gnus" 'filteredp)))


;;;###autoload
(defun bookmarkp-bmenu-list-only-region-bookmarks ()
  "Display (only) the bookmarks that record a region."
  (interactive)
  (let ((bookmark-alist  (bookmarkp-region-alist-only))
        (bookmarkp-bmenu-called-from-inside-flag t))
    (setq bookmarkp-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark+ Regions" 'filteredp)))


;;;###autoload
(defun bookmarkp-bmenu-list-only-woman-man-bookmarks ()
  "Display (only) the bookmarks that record a Man or Woman page."
  (interactive)
  (let ((bookmark-alist  (bookmarkp-woman-man-alist-only))
        (bookmarkp-bmenu-called-from-inside-flag t))
    (setq bookmarkp-latest-bookmark-alist bookmark-alist)
    (bookmark-bmenu-list "% Bookmark+ Man&Woman pages" 'filteredp)))


;;;###autoload
(defun bookmarkp-bmenu-show-all-bookmarks ()
  "Show all bookmarks without removing marks if some."
  (interactive)
  (let ((bookmarkp-bmenu-called-from-inside-flag t))
    (bookmark-bmenu-list)))


;;; *-bmenu-* Commands and functions for marked bookmarks

;;;###autoload
(defun bookmarkp-bmenu-mark-all-bookmarks ()
  "Mark all bookmarks with flag >."
  (interactive)
  (with-current-buffer "*Bookmark List*"
    (goto-char (point-min))
    (bookmark-bmenu-check-position)
    (save-excursion
      (while (not (eobp))
        (when (bookmark-bmenu-check-position)
          (bookmark-bmenu-mark)))))
  (bookmarkp-count-marked))


;;;###autoload
(defun bookmarkp-bmenu-unmark-all-deletion-flags ()
  "Unmark all bookmarks marked with flag D."
  (interactive)
  (bookmarkp-bmenu-unmark-all-1 'del)
  (bookmarkp-count-marked))

;;;###autoload
(defun bookmarkp-bmenu-unmark-all-non-deletion-flags ()
  "Unmark all bookmarks marked with flag >."
  (interactive)
  (bookmarkp-bmenu-unmark-all-1 nil 'mark)
  (bookmarkp-count-marked))


;;;###autoload
(defun bookmarkp-bmenu-unmark-all ()
  "Unmark all bookmarks marked with flag > or D.
Called with prefix arg provide an interactive interface."
  (interactive)
  (if (or (bookmarkp-current-list-have-marked-p)
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^>\\|^D" (point-max) t)))
      (progn
        (if current-prefix-arg
            (bookmarkp-bmenu-unmark-all-2)
            (bookmarkp-bmenu-unmark-all-1))
        (bookmarkp-count-marked))
      (message "Nothing to unmark here!")))


(defun bookmarkp-bmenu-unmark-all-1 (&optional del mark)
  "Unmark all bookmarks or only bookmarks marked with flag > or D.
Whitout args unmark all.
If DEL is non--nil unmark only bookmarks with flag D.
If MARK is non--nil unmark only bookmarks with flag >."
  (with-current-buffer "*Bookmark List*"
    (save-excursion
      (goto-char (point-min))
      (bookmark-bmenu-check-position)
      (while (cond (mark
                    (re-search-forward "^>" (point-max) t))
                   (del
                    (re-search-forward "^D" (point-max) t))
                   (t (re-search-forward "^>\\|^D" (point-max) t)))
        (when (bookmark-bmenu-check-position)
          (bookmark-bmenu-unmark))))))


;;;###autoload
(defun bookmarkp-bmenu-unmark-all-2 ()
  "Provide an interactive interface to unmark bookmarks."
  (with-current-buffer "*Bookmark List*"
    (let ((prompt "(U)nmark, (I)gnore, (P)recedent line, (N)ext line, (!)Unmark all remaining, (Q)uit")
          action)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^>\\|^D" (point-max) t)
        (forward-line 0)
        (catch 'break
          (while 1
            (catch 'continue
              (when (fboundp 'propertize)
                (setq prompt (propertize prompt 'face '((:foreground "cyan")))))
              (setq action (read-event prompt))
              (case action
                (?U (when (bookmark-bmenu-check-position)
                      (bookmark-bmenu-unmark)
                      (if (re-search-forward "^>\\|^D" (point-max) t)
                          (progn (forward-line 0) (throw 'continue nil))
                          (throw 'break nil))))
                (?I (if (looking-at "^>\\|^D")
                        (progn (forward-char 1)
                               (if (re-search-forward "^>\\|^D" (point-max) t)
                                   (progn (forward-line 0) (throw 'continue nil))
                                   (throw 'break nil)))
                        (throw 'break nil)))
                (?P (forward-line -1) (throw 'continue nil))
                (?N (forward-line 1) (throw 'continue nil))
                (?! (throw 'break
                      (while (re-search-forward "^>\\|^D" (point-max) t)
                        (when (bookmark-bmenu-check-position)
                          (bookmark-bmenu-unmark)))))
                (?Q (throw 'break nil))))))))))


(defun bookmarkp-count-marked ()
  "Send message with number of marked and unmarked bookmarks in current display."
  (let* ((marked   (length bookmarkp-bookmark-marked-list))
         (unmarked (- (length bookmarkp-latest-bookmark-alist) marked)))
    (message "%s Marked, %s Unmarked" marked unmarked)))


;;;###autoload
(defun bookmarkp-bmenu-regexp-mark (regexp)
  "Mark bookmarks that match REGEXP."
  (interactive "sRegexp: ")
  (with-current-buffer "*Bookmark List*"
    (goto-char (point-min))
    (forward-line 2)
    (while (re-search-forward regexp (point-max) t)
      (when (bookmark-bmenu-check-position)
        (bookmark-bmenu-mark))))
  (bookmarkp-count-marked))


;;;###autoload
(defun bookmarkp-bmenu-hide-marked ()
  "Hide all marked bookmarks."
  (interactive)
  (when (or (bookmarkp-current-list-have-marked-p)
            (bookmarkp-current-list-have-marked-p 
             bookmarkp-bmenu-before-hide-marked-list))
    (let ((bookmark-alist  bookmarkp-latest-bookmark-alist)
          (bookmarkp-bmenu-called-from-inside-flag t)
          status)
      (if bookmarkp-bmenu-before-hide-marked-list
          ;; unhide marked
          (progn
            (setq bookmark-alist bookmarkp-bmenu-before-hide-marked-list)
            (setq bookmarkp-bmenu-before-hide-marked-list nil)
            (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
            (setq status 'show))
          ;; hide marked
          (setq bookmarkp-bmenu-before-hide-marked-list bookmarkp-latest-bookmark-alist)
          (setq bookmark-alist (bookmarkp-non-marked-bookmarks-only))
          (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
          (setq status 'hiden))
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (if (eq status 'hiden)
          (bookmark-bmenu-check-position)
          (goto-char (point-min))
          (when (re-search-forward "^>" (point-max) t)
            (forward-line 0))))))


;;;###autoload
(defun bookmarkp-bmenu-hide-unmarked ()
  "Hide all unmarked bookmarks."
  (interactive)
  (when (bookmarkp-current-list-have-marked-p)
    (let ((bookmark-alist  bookmarkp-latest-bookmark-alist)
          (bookmarkp-bmenu-called-from-inside-flag t)
          status)
      (if bookmarkp-bmenu-before-hide-unmarked-list
          ;; unhide non marked
          (progn                                                  
            (setq bookmark-alist bookmarkp-bmenu-before-hide-unmarked-list)
            (setq bookmarkp-bmenu-before-hide-unmarked-list nil)
            (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
            (setq status 'show))
          ;; hide non-marked
          (setq bookmarkp-bmenu-before-hide-unmarked-list bookmarkp-latest-bookmark-alist)
          (setq bookmark-alist (bookmarkp-marked-bookmarks-only))
          (setq bookmarkp-latest-bookmark-alist  bookmark-alist)
          (setq status 'hiden))
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (if (eq status 'hiden)
          (bookmark-bmenu-check-position)
          (goto-char (point-min))
          (when (re-search-forward "^>" (point-max) t)
            (forward-line 0))))))


;;;###autoload
(defun bookmarkp-bmenu-toggle-marks ()
  "Toggle mark on each bookmark in menu-list."
  (interactive)
  (with-current-buffer "*Bookmark List*"
    (save-excursion
      (goto-char (point-min))
      (bookmark-bmenu-check-position)
      (if (bookmarkp-current-list-have-marked-p)
          (while (not (eobp))
            (let ((bmk (bookmark-bmenu-bookmark)))
              (if (member bmk bookmarkp-bookmark-marked-list)
                  (bookmark-bmenu-unmark)
                  (bookmark-bmenu-mark))))
          (bookmarkp-bmenu-mark-all-bookmarks)))
    (bookmarkp-count-marked)))


;; Predicates --------------------------------------------------------

(defun bookmarkp-region-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK has region information.
BOOKMARK is a bookmark name or a bookmark record."
  (and (bookmarkp-get-end-position bookmark)
       (/= (bookmark-get-position bookmark)
           (bookmarkp-get-end-position bookmark))))

(defun bookmarkp-gnus-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Gnus bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-gnus))

(defun bookmarkp-w3m-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a W3m bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-w3m))

(defun bookmarkp-info-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is an Info bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'Info-bookmark-jump))

(defun bookmarkp-woman-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Woman bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-woman))

(defun bookmarkp-man-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Man bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-man))

(defun bookmarkp-woman-man-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Man or Woman bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (bookmarkp-man-bookmark-p bookmark)
      (bookmarkp-woman-bookmark-p bookmark)))

(defun bookmarkp-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
  (let* ((filename   (bookmark-get-filename bookmark))
         (isnonfile  (equal filename bookmarkp-non-file-filename))) 
    (and filename (not isnonfile) (not (bookmark-get-handler bookmark)))))

(defun bookmarkp-non-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a non-file bookmark (e.g *scratch*).
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
  (let* ((filename   (bookmark-get-filename bookmark))
         (isnonfile  (equal filename bookmarkp-non-file-filename))) 
    (and isnonfile (not (bookmark-get-handler bookmark)))))

(defun bookmarkp-remote-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a remote file or directory.
BOOKMARK is a bookmark name or a bookmark record."
  (let* ((file      (bookmark-get-filename bookmark))
         (rem-file  (and file           ; Don't give nil to `file-remote-p'
                         (if (fboundp 'file-remote-p)
                             (file-remote-p file)
                             (and (fboundp 'ffap-file-remote-p) (ffap-file-remote-p file))))))
    (and rem-file  (not (bookmark-get-handler bookmark))  rem-file)))

(defun bookmarkp-local-file-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a local file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
  (and (bookmarkp-file-bookmark-p bookmark)
       (not (bookmarkp-remote-file-bookmark-p bookmark))))

(defun bookmarkp-local-directory-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a local directory.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((file  (bookmark-get-filename bookmark)))
    (and (bookmarkp-local-file-bookmark-p bookmark) (file-directory-p file))))

(defun bookmarkp-bookmark-marked-p (bookmark)
  "Return non-nil if BOOKMARK is a marked bookmark."
  (member (car bookmark) bookmarkp-bookmark-marked-list))

;; Filter Functions --------------------------------------------------

(defun bookmarkp-region-alist-only ()
  "`bookmark-alist', filtered to retain only bookmarks that have regions.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-region-bookmark-p bookmark-alist))


(defun bookmarkp-gnus-alist-only ()
  "`bookmark-alist', filtered to retain only Gnus bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-gnus-bookmark-p bookmark-alist))


(defun bookmarkp-w3m-alist-only ()
  "`bookmark-alist', filtered to retain only W3m bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-w3m-bookmark-p bookmark-alist))


(defun bookmarkp-info-alist-only ()
  "`bookmark-alist', filtered to retain only Info bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-info-bookmark-p bookmark-alist))


(defun bookmarkp-woman-alist-only ()
  "`bookmark-alist', filtered to retain only Woman bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-woman-bookmark-p bookmark-alist))


(defun bookmarkp-man-alist-only ()
  "`bookmark-alist', filtered to retain only Man bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-man-bookmark-p bookmark-alist))


(defun bookmarkp-woman-man-alist-only ()
  "`bookmark-alist', filtered to retain only Man or Woman bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-woman-man-bookmark-p bookmark-alist))


(defun bookmarkp-remote-file-alist-only ()
  "`bookmark-alist', filtered to retain only remote-file bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-remote-file-bookmark-p bookmark-alist))


(defun bookmarkp-local-file-alist-only ()
  "`bookmark-alist', filtered to retain only local-file bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-local-file-bookmark-p bookmark-alist))


(defun bookmarkp-file-alist-only (&optional hide-remote)
  "`bookmark-alist', filtered to retain only file and directory bookmarks.
This excludes bookmarks that might contain file information but are
particular in some way - for example, Info bookmarks or Gnus bookmarks.

Non-nil argument HIDE-REMOTE means do not include remote file or
directory bookmarks.

A new list is returned (no side effects)."
  (bookmarkp-remove-if
   #'(lambda (bookmark)
       (or (bookmarkp-non-file-bookmark-p bookmark)
           (bookmarkp-gnus-bookmark-p bookmark)
           (bookmarkp-w3m-bookmark-p bookmark)
           (bookmarkp-woman-man-bookmark-p bookmark)
           (bookmarkp-info-bookmark-p bookmark)
           (and hide-remote (bookmarkp-remote-file-bookmark-p bookmark))))
   bookmark-alist))


(defun bookmarkp-non-file-alist-only ()
  "`bookmark-alist', filtered to retain only non-file bookmarks.
A new list is returned (no side effects)."
  (bookmarkp-remove-if-not #'bookmarkp-non-file-bookmark-p bookmark-alist))


;;; Marked bookmarks

(defun bookmarkp-marked-bookmarks-only ()
  "Return the list of marked bookmarks."
  (bookmarkp-remove-if-not #'bookmarkp-bookmark-marked-p bookmark-alist))

(defun bookmarkp-non-marked-bookmarks-only ()
  "Return the list of not marked bookmarks."
  (bookmarkp-remove-if #'bookmarkp-bookmark-marked-p bookmark-alist))


(defun bookmarkp-current-list-have-marked-p (&optional alist)
  "Return non--nil if `bookmarkp-latest-bookmark-alist' have marked bookmarks."
  (when bookmarkp-bookmark-marked-list
    (let ((last-alist (or alist bookmarkp-latest-bookmark-alist)))
      (catch 'break
        (dolist (i last-alist)
          (when (bookmarkp-bookmark-marked-p i)
            (throw 'break t)))))))


;; Other Functions ---------------------------------------------------

(defun bookmarkp-get-buffer-name (bookmark)
  "Return the buffer-name of BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'buffer-name))

(defun bookmarkp-get-end-position (bookmark)
  "Return the end-position of REGION in BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
  (bookmark-prop-get bookmark 'end-position))

(defun bookmarkp-root-or-sudo-logged-p ()
  "Return t if the user logged in using Tramp as `root' or `sudo'.
Otherwise, return nil."
  (let ((su-or-sudo-regex  "\\(su\\|sudo\\)"))
    (catch 'break
      (dolist (i (mapcar #'buffer-name (buffer-list)))
        (when (string-match (format "*tramp/%s ." su-or-sudo-regex) i) (throw 'break t))))))

(defun bookmarkp-region-record-front-context-string (breg ereg)
  "Return the region prefix, at BREG.
Return at most `bookmarkp-region-search-size' or (- EREG BREG) chars."
  (buffer-substring-no-properties
   breg (+ breg (min bookmarkp-region-search-size (- ereg breg)))))

(defun bookmarkp-record-front-context-string (breg)
  "Return `bookmark-search-size' chars, starting at position BREG.
Return nil if there are not that many chars."
  (and (>= (- (point-max) breg) bookmark-search-size)
       (buffer-substring-no-properties breg (+ breg bookmark-search-size))))

(defun bookmarkp-region-record-rear-context-string (breg)
  "Return the text preceding the region beginning, BREG.
Return at most `bookmarkp-region-search-size' chars."
  (buffer-substring-no-properties
   (max (- breg bookmarkp-region-search-size) (point-min))
   breg))

(defun bookmarkp-record-rear-context-string (breg)
  "Return `bookmark-search-size' chars that precede BREG (inclusive).
Return nil if there are not that many chars."
  (and (>= (- breg (point-min)) bookmark-search-size)
       (buffer-substring-no-properties breg (- breg bookmark-search-size))))

(defun bookmarkp-record-front-context-region-string (breg ereg)
  "Return the region suffix, ending at EREG.
Return at most `bookmarkp-region-search-size' or (- EREG BREG) chars."
  (buffer-substring-no-properties
   (- ereg (min bookmarkp-region-search-size (- ereg breg)))
   ereg))

(defun bookmarkp-record-end-context-region-string (ereg)
  "Return the text following the region end, EREG.
Return at most `bookmarkp-region-search-size' chars."
  (buffer-substring-no-properties
   ereg (+ ereg (min bookmarkp-region-search-size (- (point-max) (point))))))

(defun bookmarkp-position-after-whitespace (position)
  "Move forward from POSITION, skipping over whitespace.  Return point."
  (goto-char position)
  (skip-chars-forward " \n\t" (point-max))
  (point))

(defun bookmarkp-position-before-whitespace (position)
  "Move backward from POSITION, skipping over whitespace.  Return point."
  (goto-char position)
  (skip-chars-backward " \n\t" (point-min))
  (point))

(defun bookmarkp-save-new-region-location (bookmark beg end)
  "Update and save `bookmark-alist' for BOOKMARK, relocating its region.
BOOKMARK is a bookmark record.
BEG and END are the new region limits for BOOKMARK.
Do nothing and return nil if `bookmarkp-save-new-location-flag' is nil.
Otherwise, return non-nil if region was relocated."
  (and bookmarkp-save-new-location-flag
       (y-or-n-p "Region relocated.  Do you want to save new region limits? ")
       (progn
         (bookmark-prop-set bookmark 'front-context-string
                            (bookmarkp-region-record-front-context-string beg end))
         (bookmark-prop-set bookmark 'rear-context-string
                            (bookmarkp-region-record-rear-context-string beg))
         (bookmark-prop-set bookmark 'front-context-region-string
                            (bookmarkp-record-front-context-region-string beg end))
         (bookmark-prop-set bookmark 'rear-context-region-string
                            (bookmarkp-record-end-context-region-string end))
         (bookmark-prop-set bookmark 'position beg)
         (bookmark-prop-set bookmark 'end-position end)
         (bookmarkp-maybe-save-bookmark)
         t)))

(defun bookmarkp-handle-region-default (bookmark)
  "Default function to handle BOOKMARK's region.
BOOKMARK is a bookmark name or a bookmark record.
Relocate the region if necessary, then activate it.
If region was relocated, save it if user confirms saving."
  ;; Relocate by searching from the beginning (and possibly the end) of the buffer.
  (let* (;; Get bookmark object once and for all.
         ;; Actually, we know BOOKMARK is a bookmark object (not a name), but play safe.
         (bmk              (bookmark-get-bookmark bookmark))
         (bor-str          (bookmark-get-front-context-string bmk))
         (eor-str          (bookmark-prop-get bmk 'front-context-region-string))
         (br-str           (bookmark-get-rear-context-string bmk))
         (ar-str           (bookmark-prop-get bookmark 'rear-context-region-string))
         (pos              (bookmark-get-position bmk))
         (end-pos          (bookmarkp-get-end-position bmk))
         (reg-retrieved-p  t)
         (reg-relocated-p  nil))
    (unless (and (string= bor-str (buffer-substring-no-properties
                                   (point) (+ (point) (length bor-str))))
                 (save-excursion
                   (goto-char end-pos)
                   (string= eor-str (buffer-substring-no-properties
                                     (point) (- (point) (length bor-str))))))
      ;; Relocate region by searching from beginning (and possibly from end) of buffer.
      (let ((beg  nil)
            (end  nil))
        ;;  Go to bob and search forward for END.
        (goto-char (point-min))
        (if (search-forward eor-str (point-max) t) ; Find END, using `eor-str'.
            (setq end  (point))
            ;; Verify that region is not before context.
            (unless (search-forward br-str (point-max) t)
              (when (search-forward ar-str (point-max) t) ; Find END, using `ar-str'.
                (setq end  (match-beginning 0)
                      end  (and end (bookmarkp-position-before-whitespace end))))))
        ;; If failed to find END, go to eob and search backward for BEG.
        (unless end (goto-char (point-max)))
        (if (search-backward bor-str (point-min) t) ; Find BEG, using `bor-str'.
            (setq beg  (point))
            ;; Verify that region is not after context.
            (unless (search-backward ar-str (point-min) t)
              (when (search-backward br-str (point-min) t) ; Find BEG, using `br-str'.
                (setq beg (match-end 0)
                      beg  (and beg (bookmarkp-position-after-whitespace beg))))))
        (setq reg-retrieved-p  (or beg end)
              reg-relocated-p  reg-retrieved-p
              ;; If only one of BEG or END was found, the relocated region is only
              ;; approximate (keep the same length).  If both were found, it is exact.
              pos              (or beg  (and end (- end (- end-pos pos)))  pos)
              end-pos          (or end  (and beg (+ pos (- end-pos pos)))  end-pos))))
    ;; Region is available. Activate it and maybe save it.
    (cond (reg-retrieved-p
           (goto-char pos)
           (push-mark end-pos 'nomsg 'activate)
           (setq deactivate-mark  nil)
           (when bookmarkp-show-end-of-region
             (let ((end-win (save-excursion
                              (forward-line (window-height))
                              (end-of-line)
                              (point))))
               ;; Bounce point and mark.
               (save-excursion (sit-for 0.6) (exchange-point-and-mark) (sit-for 1))
               ;; Recenter if region end is not visible.
               (when (> end-pos end-win) (recenter 1))))
           ;; Maybe save region.
           (if (and reg-relocated-p
                    (bookmarkp-save-new-region-location bmk pos end-pos))
               (message "Saved relocated region (from %d to %d)" pos end-pos)
               (message "Region is from %d to %d" pos end-pos)))
          (t                            ; No region.  Go to old start.  Don't push-mark.
           (goto-char pos) (forward-line 0)
           (message "No region from %d to %d" pos end-pos)))))


(defun bookmarkp-goto-position (file buf bufname pos forward-str behind-str)
  "Go to a bookmark that has no region.
Arguments are, respectively, the bookmark's file, buffer, buffer name,
position, and the context strings for the position."
  (if (and file (file-readable-p file) (not (buffer-live-p buf)))
      (with-current-buffer (find-file-noselect file) (setq buf  (buffer-name)))
      ;; No file found.  See if a non-file buffer exists for this.  If not, raise error.
      (unless (or (and buf (get-buffer buf))
                  (and bufname (get-buffer bufname) (not (string= buf bufname))))
        (signal 'file-error `("Jumping to bookmark" "No such file or directory" file))))
  (set-buffer (or buf bufname))
  (save-current-buffer (funcall bookmarkp-jump-display-function (current-buffer)))
  (setq deactivate-mark  t)
  (raise-frame)
  (goto-char pos)
  ;; Try to relocate position.
  ;; Search forward first.  Then, if FORWARD-STR exists and was found in the file, search
  ;; backward for BEHIND-STR.  The rationale is that if text was inserted between the two
  ;; in the file, then it's better to end up before point, so you can see the text, rather
  ;; than after it and not see it.
  (when (and forward-str (search-forward forward-str (point-max) t))
    (goto-char (match-beginning 0)))
  (when (and behind-str (search-backward behind-str (point-min) t))
    (goto-char (match-end 0)))
  nil)

;; W3M support
(defun bookmarkp-make-w3m-record ()
  "Make a special entry for w3m buffers."
  (require 'w3m)                        ; For `w3m-current-url'.
  `(,@(bookmark-make-record-default 'point-only)
      (filename . ,w3m-current-url)
      (handler . bookmarkp-jump-w3m)))

(add-hook 'w3m-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'bookmarkp-make-w3m-record)))

(defun bookmarkp-w3m-set-new-buffer-name ()
  "Set the w3m buffer name according to the number of w3m buffers already open."
  (let ((len  (length (w3m-list-buffers 'nosort))))
    (if (eq len 0)  "*w3m*"  (format "*w3m*<%d>" (1+ len)))))

(defun bookmarkp-jump-w3m-new-session (bookmark)
  "Jump to W3m bookmark BOOKMARK, setting a new tab."
  (let ((file  (bookmark-prop-get bookmark 'filename))
        (buf   (bookmarkp-w3m-set-new-buffer-name)))
    (w3m-browse-url file 'newsession)
    (while (not (get-buffer buf)) (sit-for 1)) ; Be sure we have the W3m buffer.
    (with-current-buffer buf
      (goto-char (point-min))
      ;; Wait until data arrives in buffer, before setting region.
      (while (eq (line-beginning-position) (line-end-position)) (sit-for 1)))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

(defun bookmarkp-jump-w3m-only-one-tab (bookmark)
  "Close all W3m sessions and jump to BOOKMARK in a new W3m buffer."
  (let ((file  (bookmark-prop-get bookmark 'filename)))
    (w3m-quit 'force)                   ; Be sure we start with an empty W3m buffer.
    (w3m-browse-url file)
    (with-current-buffer "*w3m*" (while (eq (point-min) (point-max)) (sit-for 1)))
    (bookmark-default-handler
     `("" (buffer . ,(buffer-name (current-buffer))) .
          ,(bookmark-get-bookmark-record bookmark)))))

(defun bookmarkp-jump-w3m (bookmark)
  "Handler function for record returned by `bookmarkp-make-w3m-record'.
BOOKMARK is a bookmark name or a bookmark record.
Use multi-tabs in W3m if `bookmarkp-w3m-allow-multi-tabs' is non-nil."
  (if bookmarkp-w3m-allow-multi-tabs
      (bookmarkp-jump-w3m-new-session bookmark)
      (bookmarkp-jump-w3m-only-one-tab bookmark)))

;; GNUS support.  Does not handle regions.
(defun bookmarkp-make-gnus-record ()
  "Make a bookmark entry for a Gnus buffer."
  (require 'gnus)
  (unless (and (eq major-mode 'gnus-summary-mode) gnus-article-current)
    (error "Please retry from the Gnus summary buffer")) ;[1]
  (let* ((grp   (car gnus-article-current))
         (art   (cdr gnus-article-current))
         (head  (gnus-summary-article-header art))
         (id    (mail-header-id head)))
    `(,@(bookmark-make-record-default 'point-only)
        (filename . ,bookmarkp-non-file-filename)
        (group . ,grp) (article . ,art)
        (message-id . ,id) (handler . bookmarkp-jump-gnus))))

(add-hook 'gnus-summary-mode-hook
          #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                            'bookmarkp-make-gnus-record)))

;; Raise an error if we try to bookmark from here [1]
(add-hook 'gnus-article-mode-hook
          #'(lambda () (set (make-local-variable 'bookmark-make-record-function)
                            'bookmarkp-make-gnus-record)))

(defun bookmarkp-jump-gnus (bookmark)
  "Handler function for record returned by `bookmarkp-make-gnus-record'.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((group    (bookmark-prop-get bookmark 'group))
        (article  (bookmark-prop-get bookmark 'article))
        (id       (bookmark-prop-get bookmark 'message-id))
        (buf      (bookmark-prop-get bookmark 'buffer)))
    (gnus-fetch-group group (list article))
    (gnus-summary-insert-cached-articles)
    (gnus-summary-goto-article id nil 'force)
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

;;; Woman support

(defun bookmarkp-make-woman-record ()
  "Make a bookmark entry for a Woman buffer."
  `(,@(bookmark-make-record-default 'point-only)
      (filename . ,woman-last-file-name)
      (handler . bookmarkp-jump-woman)))

(add-hook 'woman-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'bookmarkp-make-woman-record)))

(defun bookmarkp-jump-woman (bookmark)
  "Default bookmark handler for Woman buffers."
  (let* ((file (bookmark-prop-get bookmark 'filename))
         (buf  (save-window-excursion
                 (woman-find-file file) (current-buffer))))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

;;; Man Support

(defun bookmarkp-make-man-record ()
  "Make a bookmark entry for a Man buffer."
  `(,@(bookmark-make-record-default 'point-only)
      (filename . ,bookmarkp-non-file-filename)
      (handler . bookmarkp-jump-man)))


(add-hook 'Man-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'bookmarkp-make-man-record)))


(defun bookmarkp-jump-man (bookmark)
  "Default bookmark handler for Man buffers."
  (let* ((buf               (bookmark-prop-get bookmark 'buffer-name))
         (buf-lst           (split-string buf))
         (node              (replace-regexp-in-string "\*" "" (car (last buf-lst))))
         (ind               (when (> (length buf-lst) 2) (second buf-lst)))
         (Man-notify-method (case bookmarkp-jump-display-function
                              ('switch-to-buffer 'pushy)
                              ('pop-to-buffer    'friendly)
                              ('display-buffer   'quiet))))
    (man (if ind (format "%s(%s)" node ind) node))
    (while (get-process "man") (sit-for 1))
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bookmark+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark+.el ends here

