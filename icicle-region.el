;;; icicle-region.el --- 
;; 
;; Author: thierry
;; Maintainer: 
;; 
;; Created: mer. juin  3 17:40:24 2009 (+0200)
;; Version: 	$Id: icicle-region.el,v 1.5 2009/06/08 21:08:01 thierry Exp thierry $	

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
;; $Log: icicle-region.el,v $
;; Revision 1.5  2009/06/08 21:08:01  thierry
;; Fix length of string if string is smaller than icicle-region-search-size.
;;
;; Revision 1.4  2009/06/08 15:49:43  thierry
;; New variable `icicle-region-search-size'.
;;
;; Revision 1.3  2009/06/08 11:29:37  thierry
;; Save new location in `icicle-region-alist' after relocate it.
;;
;; Revision 1.2  2009/06/08 10:45:04  thierry
;; Initial commit implement code to retrieve region even if this one have moved in file.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; Note:`bookmark-search-size' is 16 and it seem sufficient. 
(defvar icicle-region-search-size 40
  "Length of the context strings recorded on either side of an icicle-region.")

(defun icicle-add-region (start end &optional tag) ; Bound to `C-N C-x C-x', N = whole number.
  "Add current region to list of regions, `icicle-region-alist'.
Save the updated option.

With a prefix argument, you are prompted for a tag to name the region.
Otherwise, the first `icicle-regions-name-length-max' characters of
the region itself serve as the name.

To remove a region from `icicle-region-alist', use command
`icicle-remove-region' or customize `icicle-region-alist'."
  (interactive "r\nP")
  (when (= start end) (error "Cannot add region it is empty"))
  (when (> start end) (setq end  (prog1 start (setq start  end))))
  (let* ((region-prefix
          (buffer-substring-no-properties start (+ start (min icicle-regions-name-length-max
                                                             (- end start)))))
         (front-region-string 
          (buffer-substring-no-properties start (+ start (min icicle-region-search-size
                                                              (- end start)))))
         (rear-region-string ;(if (> (- end start) (icicle-region-search-size))
          (buffer-substring-no-properties (- end (length front-region-string)) end)))
    (add-to-list 'icicle-region-alist
                 (list (setq tag  (if tag
                                      (icicle-completing-read-history  "Add region (tag): "
                                                                       nil nil nil region-prefix)
                                    region-prefix))
                       (buffer-name)
                       (if (string= (buffer-name) "*info*") ;(and (eq major-mode 'Info-mode)
                           (concat "(" (file-name-nondirectory Info-current-file) ")" Info-current-node)
                           (buffer-file-name))
                       start
                       end
                       front-region-string
                       rear-region-string))
    (funcall icicle-customize-save-variable-function 'icicle-region-alist icicle-region-alist)
    (message "Region added to `icicle-region-alist' with tag `%s'"
             (if (> (length tag) 20) (concat (substring tag 0 17) "...") tag))))

(defun icicle-select-region-action (reg-name)
  "Action function for `icicle-select-region'."
  (let* ((reg   (funcall icicle-get-alist-candidate-function reg-name))
         (buf   (cadr reg))
         (file  (car (cddr reg))))
    (when (and (not (get-buffer buf)) file) ; If no buffer, try to open the file.  If no file, msg.
      (cond ((string= buf "*info*") (info file))
            ((file-readable-p file) (find-file-noselect file))
            (t (message "No such file: `%s'" file))))
    (when (get-buffer buf)
      (pop-to-buffer buf)
      (raise-frame)
      (goto-char (cadr (cddr reg)))
      (let ((start-str (car (last (butlast reg))))
            (end-str (car (last reg))))
        (if (stringp start-str) ;; for compatibility with old entries
            (if (string= start-str (buffer-substring-no-properties (point) (+ (point) (length start-str))))
                (push-mark (car (cddr (cddr reg))) 'nomsg 'activate) ;; position didn't change
                ;; position have changed: relocate region.
                (goto-char (point-max)) 
                (when (re-search-backward start-str nil t)
                  (let ((beg (point))
                        end)
                    (save-excursion
                      (re-search-forward end-str nil t)
                      (setq end (point)))
                    ;; Go to the new location
                    (push-mark end 'nomsg 'activate)
                    ;; Modify `icicle-region-alist'
                    (setf (nth 3 reg) beg)
                    (setf (nth 4 reg) end)
                    ;; save `icicle-region-alist'
                    (funcall icicle-customize-save-variable-function 'icicle-region-alist icicle-region-alist)
                    (message "New location of region at %s %s saved" beg end))))
            (push-mark (car (cddr (cddr reg))) 'nomsg 'activate)))))
  (setq deactivate-mark  nil))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicle-region.el ends here
