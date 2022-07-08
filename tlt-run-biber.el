(defcustom org-biber-show-compilation t
    "Whether to show biber output buffer while compiling."
:group 'tlt)

  (defvar org-biber-process nil
    "Stores last process created by calling biber. 
      For internal use only.")

(defun tex-file-get-create (&optional opt1 opt2)
  "Get or create the `.tex' file corresponding to the current buffer's `.org' file.
       If the search is unsuccessful, create a `.tex' file. If there
    is no `.bcf' file, ask whether to run TeX to create it. Return the `.tex' file's name."
  ;; temporariy variables
  (let* ((dir (file-name-directory (buffer-file-name)))
         (texname (org-export-output-file-name ".tex")) ; without path
         (texfile (expand-file-name texname dir))
         (bound-aux (boundp 'tex-auxdir-name))
         (bcf (org-export-output-file-name ".bcf")) ; without path
         (TeX-command-sequence-max-runs-same-command 1)) ; do not repeat commands
    ;; check `.tex' file ;;
    (if (file-exists-p texfile) texfile
      (org-latex-export-to-latex))
    ;; check `.bcf' file ;;
    (cond ((and bound-aux ; variable is bound
                (file-exists-p (concat dir tex-auxdir-name bcf))) ; and bcf exists in auxdir
           (prin1-to-string (concat dir tex-auxdir-name bcf))) ; first possible output: auxdir path  bcf

          ((file-exists-p bcf) (prin1-to-string bcf)) ; second possible output: parent folder path to bcf

          (t (if 
                 (yes-or-no-p "Necessary .bcf file does not exist. Run LaTeX to create it first?") ;  ask to create such a file.
                 (progn 
                   (org-latex-export-to-pdf) ; third possible output: no `.bcf', so creat it!

                   (if (or 
                        (when bound-aux (file-exists-p (concat dir tex-auxdir-name bcf)))
                        (file-exists-p bcf))
                       (prin1-to-string  ;  escape quotation marks for the biber command to work
                        (or (concat dir tex-auxdir-name bcf) bcf))     ; (TeX-strip-extension texfile) ; strip extension bc biber only handles`.bcf' or none
                                        ; `org-latex-export-to-latex' evaluates to the file name already
                     (error "No .bcf file was produced. Did you forget to include your .bib file?"))) ; else give out message

               (error "Command aborted.")))))) ; else abort the compilation

(defun org-call-biber () 
  "Run biber on the `.tex' file corresponding to the current buffer's `.org' file and return process.
            If there is no such file, create it. If there is no `.bcf' file, ask for a LaTeX run first;
          see `tex-file-get-create'."
  (interactive)
  (let ((TeX-expand-list-builtin ; replace file name function in biber call
         (add-to-list 'TeX-expand-list-builtin '("%s" tex-file-get-create nil t))) ; prepending to list works: old entry further down is ignored
        (TeX-show-compilation org-biber-show-compilation) ; own variable whether to show output directly
        (TeX-command-sequence-max-runs-same-command 1) ; only run once
        (binding (substitute-command-keys                                     ;  store the command bindings
                  "\\<tlt-TeX-utils-map>\\[tlt-tex-switch-to-biber-ouput]"))) ;  of the buffer switch command
    (cl-letf (((symbol-function 'substitute-command-keys) (lambda (string &optional noface) binding))) ; and return it when `substitute-command-keys' is called
      (setq org-biber-process (TeX-command "Biber" #'tex-file-get-create))))) ; store the process returned to make it accessible later on

(defun tlt-org-biber-sentinel-wrapper (fun process name)
  "Show the correct key bindings to open the biber output buffer in org-mode.
  Wrapper for `TeX-Biber-sentinel'."
  (if (derived-mode-p 'org-mode)
      (let ((binding (substitute-command-keys                                 ; see above, same procedure
                      "\\<tlt-TeX-utils-map>\\[tlt-tex-switch-to-biber-ouput]")))
        (cl-letf (((symbol-function 'substitute-command-keys) (lambda (string &optional noface) binding)))
          (funcall fun process name)))
    (funcall fun process name)))

(defun tlt-tex-switch-to-biber-output ()
(interactive)
    "Display the buffer containing the output from biber.
  Infom about the file on which biber ran."
    (let* ((buf (process-buffer org-biber-process))
           (bufname (buffer-name buf))
           (name (nth 1 (split-string bufname "\""))))
      (prog1
          (message (format "Output from biber call on %s" name))
        (display-buffer buf))))

(provide 'tlt-run-biber)

;; tlt-run-biber ends here ;;
