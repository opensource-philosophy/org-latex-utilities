(defcustom tlt-remove-frame-files t 
  "Whether or not to remove the files necessary to compile the single-frame PDF."
  :group 'tlt-utils)

(defcustom tlt-remove-frame-pdf nil 
  "Whether or not to also remove the `.pdf' file of the single-frame."
  :group 'tlt-utils)

(defcustom org-latex-compile-frame-command (concat (format "%s -shell-escape -interaction nonstopmode -f" (replace-regexp-in-string "tex" "latex" (prin1-to-string TeX-engine))) " %f")
  "Command to compile a single beamer frame with in org. `%f' is the file path."
  :group 'tlt-utils)

(defun org-goto-heading-at-level (level &optional backward)
  "Go to the next org-heading of level LEVEL. 
  If BACKWARD is t, go to the last org-heading of level LEVEL."
  (let ((goal (format "^%s [^$\n]+$" 
                      (regexp-quote (make-string level ?*)))))
    (if backward
        (search-backward-regexp goal nil t)
      (search-forward-regexp goal nil t))))

(defun org-heading-geq (level)
  "Create a regexp that matches all org headlines of LEVEL or below."
  (let ((nextlev 0)
        (list nil))
    (while (< nextlev (1+ level))
      (push (make-string nextlev ?*) list)
      (setq nextlev (1+ nextlev)))
    (format "^%s [^$\n]+$" (regexp-opt list))))

(defun org-find-frame ()
  "Return a list containing the name of the frame as well as its beginning and end coordinates."
  (let* ((level (1+ org-beamer-frame-level))
         (beg (org-goto-heading-at-level level t))  ; beginning of frame
         (name (substring-no-properties (org-get-heading))) ; get properties off
               (end 
                (progn
                  (next-line)
                  (beginning-of-line)
                  (re-search-forward (org-heading-geq level))
                  (previous-line)
                  (end-of-line) ; end of frame
                  (point))))
         (list name beg end)))

(defun tlt-copy-frame ()
  (interactive)
  "Copy frame at point into the kill-ring."
  (let* ((info (org-find-frame))
         (beg (nth 1 info))
         (end (nth 2 info)))
    (copy-region-as-kill beg end)))

(defun tlt-kill-frame ()
  (interactive)
  "Delete frame at point and store it in the kill-ring."
  (let* ((info (org-find-frame))
         (beg (nth 1 info))
         (end (nth 2 info)))
    (kill-region beg end)))

(defun spot-TeX-environment (env)
  "Find TeX-environment ENV closest to point. Return
        beginning and end coordinates. If there is no such
      environment, return nil."
  (let ((beg-re (format "\\\\begin{%s}" env)))
    (when (search-forward-regexp beg-re nil t)
      (let* ((beg (progn
                    (beginning-of-line) (point)))
             (end-re (format "\\\\end{%s}" env))
             (end (progn
                    (search-forward-regexp end-re nil t))))
        (list beg end)))))

(defun delete-beamer-frames ()
  "Delete all LaTeX frame environments from point onwards."
  (while (search-forward-regexp "\n\\\\begin{frame}" nil t)
    (beginning-of-line)
    (backward-char 1)
    (let* ((bounds  (spot-TeX-environment 'frame))
           (beg (nth 0 bounds))
           (end (nth 1 bounds)))
      (delete-region beg end))))

(defun insert-beamer-frame-headings (level)
  "Insert a hierarchy of headings from the top-level-heading
            to a heading with level LEVEL."
  (let ((count 1))
    (while (<= count level)
      (insert (format "%s Heading Level %s"
                      (make-string count ?*) count))
      (newline)
      (setq count (1+ count)))))

(defun tlt-frames-remove (name)
  "Remove files necessary to compile frame."
  (let* ((bare (file-name-base name))
         (dir (file-name-directory name))
         (dirfiles (directory-files dir))
         (auxdir (if (boundp 'tlt-auxdir-name) 
                     (concat dir tlt-auxdir-name) ""))
         (auxdirfiles (directory-files auxdir))
         (pdf-maybe (if (not tlt-remove-frame-pdf) (concat bare ".pdf") "")))

    (let ((value))
      (dolist (element auxdirfiles value)
        (when (and (string-match 
                    (regexp-quote bare) 
                    (regexp-quote element))
                   (not (member element 
                                `("." ".." ,pdf-maybe))))
          (push element value)))

      (when value
        (let ((aux-delete
               (mapcar (lambda (x) (concat auxdir x)) value)))
          (mapc #'delete-file aux-delete))))

    (let ((value2))
      (dolist (element dirfiles value2)
        (when (and (string-match (regexp-quote bare) (regexp-quote element))
                   (not (member element 
                                `(list "." ".." ,pdf-maybe))))
          (push element value2))) 

      (when value2
        (let ((main-delete
               (mapcar (lambda (x) (concat dir x)) value2)))
          (mapc #'delete-file main-delete))))))

(defun tlt-compile-beamer-frame ()
  (interactive)
  (let* ((beamer-level (1+ org-beamer-frame-level)) ; +1 because we dont count from 0
         (level (make-string beamer-level ?*)) ; headline level for frames in stars
         (frame-info (org-find-frame))

         (dir (file-name-directory buffer-file-name))
         (buf-name (file-name-nondirectory buffer-file-name)) ; buffer name 
         (frame-name (substring-no-properties (car frame-info))) ; get away with org properties
         (frame-file (format "[%s] %s" frame-name buf-name))
         (filename (expand-file-name frame-file dir))
         (frame (buffer-substring (nth 1 frame-info) (nth 2 frame-info))) ; single org frame  
         (org-export-with-title nil) ; do not export title frame   
         (org-latex-pdf-process (list org-latex-compile-frame-command)))

    (with-current-buffer (org-export-copy-buffer) ; copy the current buffer to a temporary one
      (org-export-expand-include-keyword) ; actually add the code from included files
      (goto-char (point-min)) ; go to beginning of buffer
      (org-goto-heading-at-level beamer-level) ; go to the first frame
      (org-goto-heading-at-level org-beamer-frame-level t) ; go to the section the frame is located
      (let ((beg (point))
            (end (point-max)))
        (delete-region beg end)
        (newline)
        (goto-char (point-min)) ; go to the file's beginning
        (delete-beamer-frames) ; delete all manually added beamer frames (for title page/agenda)
        (goto-char (point-max))
        (insert-beamer-frame-headings org-beamer-frame-level) ; add place-holder headlines
        (insert frame) ; insert the frame
        (write-file frame-file) ; write the buffer to a file
        (org-open-file (org-beamer-export-to-pdf))) ; and open the file after compilation
      (when tlt-remove-frame-files (tlt-frames-remove filename))
      )))

(provide 'tlt-compile-frame)

;; tlt-compile-frame ends here ;;
