;;; Compatibility Code for Older Emacs Versions ----------------------

;;;###autoload
(when (< emacs-major-version 23)

  ;; These definitions are for Emacs versions prior to Emacs 23.
  ;; They are the same as the vanilla Emacs 23+ definitions, except as noted.
  ;; They let older versions of Emacs handle bookmarks created with Emacs 23.

  (defun bookmark-get-bookmark (bookmark &optional noerror)
    "Return the bookmark record corresponding to BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
If BOOKMARK is already a bookmark record, just return it,
Otherwise look for the corresponding bookmark in `bookmark-alist'."
    (cond
      ((consp bookmark) bookmark)
      ((stringp bookmark)
       (or (if (fboundp 'assoc-string)  ; Emacs 22+.
               (assoc-string bookmark bookmark-alist bookmark-completion-ignore-case)
               (assoc bookmark bookmark-alist))
           (unless noerror (error "Invalid bookmark: `%s'" bookmark))))))

  (defun bookmark-get-bookmark-record (bookmark)
    "Return the guts of the entry for BOOKMARK in `bookmark-alist'.
That is, all information but the name.
BOOKMARK is a bookmark name or a bookmark record."
    (let ((alist  (cdr (bookmark-get-bookmark bookmark))))
      ;; The bookmark objects can either look like (NAME ALIST) or
      ;; (NAME . ALIST), so we have to distinguish the two here.
      (if (and (null (cdr alist)) (consp (caar alist)))
          (car alist)
          alist)))

  (defun Info-bookmark-make-record ()
    "Create an Info bookmark record."
    `(,Info-current-node
      ,@(bookmark-make-record-default 'point-only)
      (filename . ,Info-current-file)
      (info-node . ,Info-current-node)
      (handler . Info-bookmark-jump)))

  ;; Requires `info.el' explicitly (not autoloaded for `Info-find-node'.
  (defun Info-bookmark-jump (bookmark)
    "Jump to Info bookmark BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
    (require 'info)
    ;; Implements the `handler' for the record type returned by `Info-bookmark-make-record'.
    (let* ((file       (bookmark-prop-get bookmark 'filename))
           (info-node  (bookmark-prop-get bookmark 'info-node))
           (buf
            (save-window-excursion      ; VANILLA EMACS FIXME: doesn't work with frames!
              (Info-find-node file info-node) (current-buffer))))
      ;; Use `bookmark-default-handler' to move to appropriate location within Info node.
      (bookmark-default-handler
       `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark)))))

  (add-hook 'Info-mode-hook (lambda ()
                              (set (make-local-variable 'bookmark-make-record-function)
                                   'Info-bookmark-make-record)))

  (defvar bookmark-make-record-function 'bookmark-make-record-default
    "Function called with no arguments, to create a bookmark record.
It should return the new record, which should be a cons cell of the
form (NAME . ALIST) or just ALIST, where ALIST is as described in
`bookmark-alist'.  If it cannot construct the record, then it should
raise an error.

NAME is a string that names the new bookmark.  NAME can be nil, in
which case a default name is used.

ALIST can contain an entry (handler . FUNCTION) which sets the handler
to FUNCTION, which is then used instead of `bookmark-default-handler'.
FUNCTION must accept the same arguments as `bookmark-default-handler'.

You can set this variable buffer-locally to enable bookmarking of
locations that should be treated specially, such as Info nodes, news
posts, images, pdf documents, etc.")

  (defun bookmark-make-record ()
    "Return a new bookmark record (NAME . ALIST) for the current location."
    (let ((record  (funcall bookmark-make-record-function)))
      ;; Set up default name.
      (if (stringp (car record))
          record                        ; The function already provided a default name.
          (when (car record) (push nil record))
          (setcar record  (or bookmark-current-bookmark (bookmark-buffer-name)))
          record)))

  (defun bookmark-store (bookmark-name alist no-overwrite)
    "Store the bookmark named bookmark-NAME, giving it data ALIST.
If NO-OVERWRITE is non-nil and another bookmark of the same name already
exists in `bookmark-alist', then record the new bookmark but do not
discard the old one."
    (bookmark-maybe-load-default-file)
    (let ((stripped-name  (copy-sequence bookmark-name)))
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
      (bookmarkp-maybe-save-bookmark)
      (setq bookmark-current-bookmark  stripped-name)
      (bookmark-bmenu-surreptitiously-rebuild-list)))

  (defun bookmark-prop-get (bookmark prop)
    "Return property PROP of BOOKMARK, or nil if no such property.
BOOKMARK is a bookmark name or a bookmark record."
    (cdr (assq prop (bookmark-get-bookmark-record bookmark))))

  (defun bookmark-get-handler (bookmark)
    "Return the `handler' entry for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record."
    (bookmark-prop-get bookmark 'handler))

  (defun bookmark-jump-noselect (bookmark)
    "Return the location recorded for BOOKMARK.
BOOKMARK is a bookmark name or a bookmark record.
The return value has the form (BUFFER . POINT), where BUFFER is a
buffer and POINT is the location within BUFFER."
    (save-excursion (bookmark-handle-bookmark bookmark) (cons (current-buffer) (point))))

  (defun bookmark-handle-bookmark (bookmark)
    "Call BOOKMARK's handler, or `bookmark-default-handler' if it has none.
Changes the current buffer and point.
Returns nil or signals a `file-error'.
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
                    (message "Bookmark not relocated \(%s\)." bookmark)
                    (signal (car err) (cdr err)))))))))
    (when (stringp bookmark) (setq bookmark-current-bookmark bookmark))
    nil))                               ; Return nil if no error.

;;;###autoload
(when (< emacs-major-version 22)

  ;; These definitions are for Emacs versions prior to Emacs 22.
  ;; They are the same as the vanilla Emacs 22+ definitions, except as noted.

  ;; 22+ just uses `bookmark-jump-other-window' for the menu also.
  (defun bookmarkp-menu-jump-other-window (event)
    "Jump to BOOKMARK (a point in some file) in another window.
See `bookmark-jump-other-window'."
    (interactive "e")
    (bookmark-popup-menu-and-apply-function 'bookmark-jump-other-window
                                            "Jump to Bookmark (in another window)" event))

  (defun bookmark-maybe-message (fmt &rest args)
    "Apply `message' to FMT and ARGS, but only if the display is fast enough."
    (when (>= baud-rate 9600) (apply 'message fmt args))))
