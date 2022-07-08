(defcustom tlt-OCLC-biber t 
  "Whether or not to remove OCLCs from your `.bib' file on every biber run."
  :group 'tlt-utils)

(defcustom tlt-OCLC-org nil 
  "Whether or not to remove OCLCs from your `.bib' file on every org export."
  :group 'tlt-utils)

(defun tlt-remove-OCLCs-buffer ()
  "Remove all OCLCs in the annotation fields of the `bibtex' file 
in the current buffer. 
  If the only entry of that annotation field is the OCLC, remove the
  whole field.
Loops over `tlt-remove-OCLC-from-entry'."
  (interactive)
  (bibtex-set-dialect 'BibTeX t)
  (bibtex-map-entries #'tlt-remove-OCLC-from-entry))

(defun tlt-remove-OCLCs-file (filename)
  "Remove all OCLCs in the annotation fields of a specified .bib FILE. 
  If the only entry of that annotation field is the OCLC, remove the
  whole field."
  (with-temp-buffer                                     ; in a temporary buffer,
    (insert-file filename)                              ; insert the contents of FILE
    (tlt-remove-OCLCs-buffer)                           ; remove the OCLCs in there
    (write-file filename)))                              ; and save the changes

(defun tlt-get-bibliography-files-buffer (&optional backend)
  "Find the bibliography files relevant for compiling 
    the LaTeX document in the current buffer. With optional
  argument, find those bibliography files for FILE. Mostly
  copied from `reftex-locate-bibliography-files'."
  (let ((files))                            ; temporarily create the empty variable `file'
    (goto-char (point-min))                 ; go the the beginning of the buffer
    (while 		                          ; ans as long as
        (re-search-forward                  ; searching forward
         (concat                            ; one of
          "\\(^\\)[^%\n\r]*\\\\\\("         ; the `reftex-bibliography-commands'
          (mapconcat #'identity 
                     '("bibliography" "nobibliography" "addbibresource") "\\|") ; is found
          "\\)\\(\\[.+?\\]\\)?{[ \t]*\\([^}]+\\)")
         nil t)                             ;  show error messages but return nil instead
      (setq files                           ; set the files 
            (append files                   ; to be a list to which the results is appened
                    (split-string (reftex-match-string 4) ; whilst the irrelevant parts of the match
                                  "[ \t\n\r]*,[ \t\n\r]*")))) ; are deleted
    (delete-dups files)))                 ; and return the list of these files, removing duplicates

(defun tlt-remove-OCLCs-auto (&optional backend subtreep visible-only body-only ext-plist)
  (interactive)
  "Automatically find the bibliography files relevant for compiling 
   the LaTeX document at hand and remove all OCLC entries from them."
  (mapc #'tlt-remove-OCLCs-file 
        (tlt-get-bibliography-files-buffer)))

(defun tlt-OCLC-add-advice ()
  "Activate automatic removal of OCLCs."
  (when tlt-OCLC-biber
    (advice-add 'TeX-run-BibTeX :before #'tlt-remove-OCLCs-auto) 
    (advice-add 'TeX-run-Biber  :before #'tlt-remove-OCLCs-auto))
  (when tlt-OCLC-org
    (add-hook 'org-export-before-parsing-hook #'tlt-remove-OCLCs-auto)))

(defun tlt-OCLC-remove-advice ()
  "Activate automatic removal of OCLCs."
  (advice-remove 'TeX-run-BibTeX #'tlt-remove-OCLCs-auto) 

  (advice-remove 'TeX-run-Biber #'tlt-remove-OCLCs-auto)

  (remove-hook 'org-export-before-parsing-hook #'tlt-remove-OCLCs-auto))

(provide 'tlt-remove-OCLC)

;; tlt-remove-OCLC ends here ;;
