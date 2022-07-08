(defcustom tlt-tidy-aux-org 'all
  "Whether to automatically create a sub-folder in which the auxiliary files are placed
      when exporting from `.org' to `.pdf' via LaTeX. If non-nil, set `org-latex-remove-logfiles' to nil.
      Further, if set to t, move all files matching one of the extensions specified by
      `org-latex-logfiles-extensions' to that folder. If 'all, move any LaTeX extension there. 
  If nil, set to nil, do not move any files. See `tlt-org-latex-export-to-pdf' for more information."
  :group 'tlt-utils)

(defcustom tlt-tidy-aux-tex 'all
  "Whether to automatically create a sub-folder in which the auxiliary files are placed
      when compiling a `.tex' file. If non-nil, set `org-latex-remove-logfiles' to nil.
      If set to t, move all files matching one of the extensions specified by 
      `org-latex-logfiles-extensions' to that folder. If set to 'all, move any LaTeX 
      extension there. If nil, set to nil, do not move any files.  see `tlt-aux-regexp-base'."
  :group 'tlt-utils)

(defcustom tlt-move-tex-to-aux-files t
  "Whether to also move the .tex file to the auxiliary directory or not
      when exporting from `.org' to `.tex'."
  :group 'tlt-utils)

(defcustom tlt-auxdir-name "auxiliary files/"
  "Name of the subdirectory created upon export from org to LaTeX;
      see `tlt-org-latex-export-to-pdf'"
  :group 'tlt-utils)

(defcustom tlt-aux-regexp-free '("preamble.fmt$" "preamble.log" "xelatex$") ; replace tlt-aux-regexp-base
  "Any file matching one of these regexps is moved."
  :group 'tlt-utils)

(defvar tlt-aux-regexp-base '("run.xml" "ind" "ilg" "lof" "lot" "tex~" "aux" "idx"
                              "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
                              "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "bcf" "synctex.gz" 
                              "ldf" "tfm" "pk" "gf" "pl" "mf" "vf" "vpl" "pfb" "afm" "map" "enc"
                              "fd"  "mtx" "etx" "gls" "glo" "glg" "acr" "acn" "alg" "ent" "gst"
                              "ist" "loa" "snm" "vrb" ".out.ps" "w18" "mtc" "mld" "mlo" "thm"
                              "xref" "4ct" "4tc" "tpt" "tpm" "tno" "los" "emd" "enx" "ent"
                              "auxx" "hd" "idv" "lg" "url" "ttt" "fff" "rsp.aux" "ovr" "lbl"
                              "ctn" "emd" "enx" "cb" "cb2" "sbb" "filelist" "scn" "abb" "BibTeX.txt"
                              "upa" "upb")
  "Files ending in one of these regexps are only moved if they 
      also start with the `.org' file's basename. (Almost) all auxiliary file extensions that LaTeX produces;
      see https://github.com/wspr/latex-auxfiles/blob/master/auxfiles.tex")

(defun tlt-concat-aux-regexps (list)
  "Make a distjunctive regular expression of the elements of LIST."
  (compat-string-trim-right ; to trim of the last "\\|"; else RE matches everything!
   (let ((value))
     (dolist (element list value)
       (setq value (concat element "\\|" value)
             ))) 
   "\\\\|"))

(defun tlt-regexp-base-name (basename exts) 
  "Return the regexp matching any file name
  starting with BASENAME and ending with one of EXTS."
  (format "^%s[^ $]+\\(%s\\)$" basename 
          (tlt-concat-aux-regexps exts)))

(defun tlt-find-files-regexp (re &optional dir)
  "Return a list of all files in RE matching DIR.
    If DIR is not provided, resort to the current buffer's directory."
  (let* ((dir (if dir 
                  dir
                (file-name-directory (buffer-file-name))))
         (dirfiles (directory-files dir))
         (files (cl-set-difference dirfiles '("." ".."))))
    (-flatten (mapcar (lambda (x) 
                        (when (string-match re x) 
                          x)) 
                      files))))

(defun tlt-move-files-regexp (re dir1 dir2)
  "Move all files in DIR1 matching RE into DIR2."
  (mapcar (lambda (x) 
            (rename-file (expand-file-name x dir1) dir2 t))
          (tlt-find-files-regexp re dir1))) ; apply it to the list of regexp matches

(defun tlt-utils-move-aux-files (direction)
  "If DIRECTION is 'up, move all auxiliary files in the main folder. Else move them in the subfolder.
If the subfolder does not exist, createit before moving. 
The sub folder name is specified by `tlt-auxdir-name.'"
  (when tlt-tidy-aux-tex                                                           ; only run if `tlt-tidy-aux-tex' is non-nil
    (let* ((basename (file-name-base (buffer-file-name)))
           (direct (file-name-directory buffer-file-name))                       ; name of the current directory (working directory)
           (aux-dir (expand-file-name                                            ; the path of the sub-folder containing the auxilary files
                     (file-name-as-directory tlt-auxdir-name) direct))           ; is the concatenation of current directory and the value of `tlt-auxdir-name', ensuring backslash!
           (aux-extension-var (if (equal tlt-tidy-aux-tex 'all)                  ; the variable to use for the LaTeX extensions is
                                  tlt-aux-regexp-base                    ; `tlt-aux-regexp-base' if `tlt-tidy-aux-org' is set to 'all
                                org-latex-logfiles-extensions))                  ; and `org-latex-logfiles-extensions' else; the nil-case is excluded by `tlt-auxiliary-files-org'
           (aux-extension-regexp (tlt-regexp-base-name basename aux-extension-var))      ; regexp dependent on file name
           (aux-free-regexp (tlt-concat-aux-regexps tlt-aux-regexp-free))           ; regexp independent of file name 

           (minted-regexp (concat "_minted-" (s-replace-regexp                ; a list of the minted folder's name is "_minted-" plus
                                              " " "_"  (file-name-base (buffer-file-name))))) ; the .org file's usual basename, but with underscores replacing whitespace

           (regexp-final (format "\\(\\(%s\\)\\|\\(%s\\)\\)\\|\\(%s\\)" 
                                 aux-extension-regexp 
                                 aux-free-regexp
                                 minted-regexp))

           (idx-file nil) 
           (element nil))

      ;;  move the files out, or create the subdirectory ;;

      (unless                                                       ; if the aux-dir does not exist yet
          (file-directory-p aux-dir)                                            ; create it
        (mkdir aux-dir))

      (if (equal direction 'up)                                 ; if 'up is provided
          (tlt-move-files-regexp regexp-final aux-dir direct)   ; box the files up
        (tlt-move-files-regexp regexp-final direct aux-dir)))    ; else move the files down                 
    nil)) ; and return nil (remember this function also serves as a hook)

(defun tlt-utils-box-up-aux-files (_)
  (interactive "p")
  "Move all LaTeX auxiliary files into a subfolder. If such a folder does not exist, create it first. 
The subfolder's name is specified by `tlt-auxdir-name'; see `tlt-utils-move-aux-files'."        
  (tlt-utils-move-aux-files 'down))

(defun tlt-utils-unbox-aux-files ()
  (interactive)
  "Move the relevant axuiliary files from the subfolder specified by `tlt-auxdir-name' to the main folder of the current file.
The relevant auxfiles are determined by `tlt-aux-regexp-free' and, in addition, 
`tlt-aux-regexp-base' or `org-latex-logfiles-extensions', depending on `tlt-tidy-aux-org' 
if in org-mode and `tlt-tidy-aux-tex' if in TeX-mode; see `tlt-utils-move-aux-files'."
  (tlt-utils-move-aux-files 'up))

(defun tlt-org-latex-compile (fun texfile &optional snippet)
  "Work just like `org-latex-compile' but create a sub-directory 
      in which all the auxiliary files necessary for compiling the
      TeX-document are stored. If such a directory already exists,
      use the files in there to compile the `.tex'-file. Provides the same
      functionality as `tlt-utils-box-up-aux-files' and `tlt-utils-unbox-aux-files'
      provide for `.tex' files. "
  (let ((tlt-tidy-aux-tex tlt-tidy-aux-org)) ; here, `tlt-tidy-aux-org' is the relevant variable
    (if tlt-move-tex-to-aux-files 

        ;; if the `.tex' file should be moved ;;

        (let ((tlt-aux-regexp-free 
               (append (list (org-export-output-file-name ".tex"))  ; add the `.tex' file to the free regexps to make sure it is moved
                       tlt-aux-regexp-free)))
          (prog1                              ; because we need the output of `org-latex-compile'
              (funcall fun texfile snippet)
            (tlt-utils-move-aux-files 'down)))

      ;; if the `.tex' file should not be moved ;;

      (tlt-utils-move-aux-files 'up)
      (prog1                               ; because we need the output of `org-latex-compile'
          (funcall fun texfile snippet)
        (tlt-utils-move-aux-files 'down)))))

(defun tlt-auxiliary-files-org-add-advice-maybe ()
    "Tell org how to handle auxiliary files while
  exporting, depending on the value of `tlt-tidy-aux-org'."
    (when tlt-tidy-aux-org                       
      (advice-add 'org-latex-compile :around 
                  #'tlt-org-latex-compile)))   

  (defun tlt-auxiliary-files-org-remove-advice ()
    "Tell org how to handle auxiliary files while
  exporting, depending on the value of `tlt-tidy-aux-org'."
    (advice-remove 'org-latex-compile      
                   #'tlt-org-latex-compile)) ; just returns nil if there is no advice to remove

  (defun tlt-utils-box-up-aux-files-wrapper (name file &optional result)
    "Wrapper for `tlt-utils-box-up-aux-files' to make it
          compatible with TeX sentinel functions. 
          Optional RESULT for `TeX-synchronous-sentinel'."
    (tlt-utils-box-up-aux-files nil))

  (defun tlt-utils-unbox-aux-files-wrapper (name file-fn &optional override-confirm)
    "Wrapper for `tlt-utils-unbox-aux-files'. 
          Makes it compatible with `TeX-command'."
    (unless TeX-command-next (tlt-utils-unbox-aux-files)))

          ;;; TeX-run-command ausprobieren, darauf basieren die ja alle!
                                          ; (advice-add 'TeX-command      :before #'tlt-utils-unbox-aux-files)
  (defun tlt-auxiliary-files-tex-add-advice-maybe ()
    "Add advice for TeX-functions to support (un-)boxing auxiliary files."
    (when tlt-tidy-aux-tex
      ;; Advice to move files out before running LaTeX/BibTeX/biber etc. ;;
      (advice-add 'TeX-command :before #'tlt-utils-unbox-aux-files-wrapper) 
      ;; Advices to move fils back in again after running LaTeX/BibTeX/biber etc. ;;
                                          ; (advice-add 'LaTeX-auto-cleanup :before #'tlt-utils-box-up-aux-files)
      (add-hook 'TeX-after-compilation-finished-functions #'tlt-utils-box-up-aux-files) ; working for TeX/LaTex runs, but not for bibtex etc.
      (advice-add 'TeX-synchronous-sentinel :before #'tlt-utils-box-up-aux-files-wrapper) ; if process is synchronous
      (advice-add 'TeX-command-sequence-sentinel :after #'tlt-utils-box-up-aux-files-wrapper) ; if `TeX-command-sequence' is called
      (advice-add 'TeX-BibTeX-sentinel   :before #'tlt-utils-box-up-aux-files-wrapper)
      (advice-add 'TeX-Biber-sentinel    :before #'tlt-utils-box-up-aux-files-wrapper)
      (advice-add 'TeX-index-sentinel    :before #'tlt-utils-box-up-aux-files-wrapper)
      (advice-add 'TeX-dvips-sentinel    :before #'tlt-utils-box-up-aux-files-wrapper)
      (advice-add 'TeX-dvipdfmx-sentinel :before #'tlt-utils-box-up-aux-files-wrapper)
      (advice-add 'TeX-ps2pdf-sentinel   :before #'tlt-utils-box-up-aux-files-wrapper)
      ))

  (defun tlt-auxiliary-files-tex-remove-advice ()
    "Remove the advices issued by `tlt-auxiliary-files-tex-add-advice-maybe'."
    ;; Advice to move files out before running LaTeX/BibTeX/biber etc. ;;
    (advice-remove 'TeX-command #'tlt-utils-unbox-aux-files-wrapper) 
    ;; Advices to move fils back in again after running LaTeX/BibTeX/biber etc. ;;
    (remove-hook 'TeX-after-compilation-finished-functions #'tlt-utils-box-up-aux-files) 
    (advice-remove 'TeX-synchronous-sentinel #'tlt-utils-box-up-aux-files-wrapper) 
    (advice-remove 'TeX-command-sequence-sentinel #'tlt-utils-box-up-aux-files-wrapper)
    (advice-remove 'TeX-BibTeX-sentinel    #'tlt-utils-box-up-aux-files-wrapper)
    (advice-remove 'TeX-Biber-sentinel     #'tlt-utils-box-up-aux-files-wrapper)
    (advice-remove 'TeX-Biber-sentinel     #'tlt-utils-box-up-aux-files-wrapper)
    (advice-remove 'TeX-index-sentinel     #'tlt-utils-box-up-aux-files-wrapper)
    (advice-remove 'TeX-dvips-sentinel     #'tlt-utils-box-up-aux-files-wrapper)
    (advice-remove 'TeX-dvipdfmx-sentinel  #'tlt-utils-box-up-aux-files-wrapper)
    (advice-remove 'TeX-ps2pdf-sentinel    #'tlt-utils-box-up-aux-files-wrapper))

(provide 'tlt-tidy-dir)

;; tlt-tidy-dir.el ends here ;;
