(defcustom precompile-engine (replace-regexp-in-string "tex" "latex" (prin1-to-string TeX-engine))
  "Engine to run precompiling command with.")

(defvar precompile-command (format "%s -ini -interaction nonstopmode -shell-escape -jobname=\"preamble\" \"&%s\\dump\"" 
                                   precompile-engine (concat precompile-engine " %b"))
  "Command to run for precompiling the preamble.")

(defvar precompile-list-item `("Precompiling" ,precompile-command TeX-run-precompile nil
                               (plain-tex-mode latex-mode doctex-mode org-mode)
                               :help "Precompile the preamble at hand for faster compiling.")
  "List for precompilation.")

(defcustom tlt-show-precompilation nil
  "Whether or not to show the output buffer of the precompilation."
  :group 'tlt)

(defvar tlt-tex-precompile-process nil
  "Stores the precompilation process. 
  For internal use only.")

(defvar preamble-string "preamble.tex")

(defun preamble-string ()
  "Returns `preamble-string'."
  preamble-string)

(defun find-preamble (&optional open base)
  "Assumes only one preamble to be in the directory or any of the subdirectories.
If called interactively, open the preamble."
  (interactive "p")
  (let* ((dir (file-name-directory (buffer-file-name)))
         (dirfiles (directory-files dir))
         (subdirs 
          (-non-nil (mapcar 
                     (lambda (x) 
                       (when (and (file-directory-p x) 
                                  (not (member  x '("." ".."))))
                         x))
                     dirfiles)))
         (dirs (push dir subdirs))) ; dir appended so that if two preambles exist, the one in the current folder is selected

    (let ((value))
      (dolist (element dirs value)
        (when (file-exists-p (expand-file-name preamble-string element))
          (setq value element)))

      (cond 

       ((and value open)
        (find-file (expand-file-name preamble-string value))
        (if (member preamble-string dirfiles)
            (message "Preamble found in current directory.")
          (message "Preamble found in subdirectory \"%s/\"." value)))

       ((and (not value)  open)
        (message "No preamble found in the subdirectories or file directory."))

       ((and value (not open))
        (if base preamble-string (expand-file-name preamble-string value)))

       ((and (not value) (not open)) (error "No existing preamble found"))))
    ))

(defun tlt-tex-precompile-preamble-sentinel (_process _name)
  "Cleanup TeX output buffer after running Biber."
  (goto-char (point-max))
  (let           ((binding (substitute-command-keys                                     ;  store the command bindings
                            "\\<tlt-TeX-utils-map>\\[tlt-tex-switch-to-precompilation-ouput]"))) ;  of the buffer switch command)
    (cl-letf (((symbol-function 'substitute-command-keys) (lambda (string &optional noface) binding)))

      (cond

       ((re-search-backward "exited abnormally\\|^Emergency stop\\|! I can't find file" nil t)
        (message (concat "TeX exited abnormally!"
                         "Type `%s' to display output.")
                 (substitute-command-keys
                  "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))

       ((re-search-backward "! Can't \dump a format with native fonts or font-mappings." nil t)
        (message "Errors while recompiling! XeLaTeX can't handle native fonts. Remove fontspec and packages that depend on it from the preamble and recompile."))

       ((re-search-backward "^INFO - \\(WARNINGS\\|ERRORS\\): \\([0-9]+\\)" nil t)
        (message (concat "Precompilation finished with %s %s. "
                         "Type `%s' to display output.")
                 (match-string 2) (downcase (match-string 1))
                 (substitute-command-keys
                  "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]"))
        (setq TeX-command-next TeX-command-default))

       ((re-search-backward "^FATAL" nil t)
        (message (concat "There was a fatal error during precompiling!"
                         "Type `%s' to display output.")
                 (substitute-command-keys
                  "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]"))
        (setq TeX-command-next "Precompiling"))

       (t
        (message "Precompiling finished successfully! You can run TeX as usual now.")
        (setq TeX-command-next TeX-command-default))))))

(defun TeX-run-precompile (name command file)
  "Create a process for NAME using COMMAND to precompile FILE.
        Return that process."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'tlt-tex-precompile-preamble-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun directory-wrapper (fun name command file)
  "Wrapper for `TeX-command-run' to change the process buffer's default directory."
  ;; If we are in the middle of a precompiling command, ;;

  (if (equal (car TeX-expand-list-builtin) '("%b" find-preamble nil t))

      ;; let `TeX-master-directory' return the preamble's path

      (flet ((TeX-master-directory () (file-name-directory (find-preamble))))
        (funcall fun name command file))

    ;; else just call `TeX-master-directory' unaltered

    (funcall fun name command file)))

(defun tlt-tex-precompile-preamble () 
  "Run biber on the `.tex' file corresponding to the current buffer's `.org' file and return process.
If there is no such file, create it. If there is no `.bcf' file, ask for a LaTeX run first;
see `tex-file-get-create'."
  (interactive)
  (let ((preamble (find-preamble)))

    (cond (preamble ; found

           (let* ((TeX-expand-list-builtin ; replace file name function in biber call
                   (add-to-list 'TeX-expand-list-builtin '("%b" find-preamble nil t))) ; prepending to list works; old entry further down is ignored
                  (TeX-show-compilation tlt-show-precompilation) ; own variable whether to show output directly
                  (TeX-command-sequence-max-runs-same-command 1) ; only run once
                  (pre-dir (file-name-directory preamble))
                                        ;  (default-directory pre-dir) ; change to directory the preamble is located in
                  (binding (substitute-command-keys                                     ;  store the command bindings
                            "\\<tlt-TeX-utils-map>\\[tlt-tex-switch-to-precompilation-output]"))) ;  of the buffer switch command
             (cl-letf (((symbol-function 'substitute-command-keys) (lambda (string &optional noface) binding))) ; and return it when `substitute-command-keys' is called

               (setq tlt-tex-precompile-process (TeX-command "Precompiling" #'preamble-string))))) ; store the process returned to make it accessible later on

          ((called-interactively-p) (message "No preamble to compile was found.")) ; not found, interactive
          (t nil)))) ; not found, not interactive

(defun tlt-tex-switch-to-precompilation-output ()
  (interactive)
  "Display the buffer containing the output from the precompilation.
      Inform where the precompilation happened."
  (let* ((buf (process-buffer tlt-tex-precompile-process))
         (bufname (buffer-name buf))
         (strlist (split-string bufname "/" t nil))
         (names (last strlist 2))
         (filename (nth 1 names))
         (filename-finish (string-trim filename nil " output\\*"))
         (dirname (concat (nth 0 names) "/")))
    (prog1
        (message (format "Output from precompilation on %s in %s" filename-finish dirname))
      (display-buffer buf))))

(defvar precompile-button                      '(precompile :image "gv"
                                                            :command (tlt-tex-precompile-preamble)
                                                            :help (lambda (&rest ignored)
                                                                    (TeX-bar-help-from-command-list "Precompile your preamble"))))

(defcustom tlt-precompile-button-tb t
  "Whether to add a precompilation button in the TeX-mode tool-bar.")

(defun tlt-preamble-template (fun contents info)
    (let* ((preamble-string "preamble.fmt") ; to not search for preamble.tex
           (preamble (condition-case nil ; condition-case handles error message
                         (find-preamble) ; find preamble.fmt
                       (error nil)))     ; return nil on error
(file (buffer-name))
           (dir (file-name-directory (buffer-file-name))))
      (if (and preamble (not (equal file "preamble.org"))) ; preamble shouldn't have features below
          ;; if there is preamble.fmt
          (progn
                                          ; move it in the main folder
            (rename-file preamble dir t)
            ;; add "%&preamble\n" and delete the documentclass definitoin
            (let ((template (funcall fun contents info)))
              (concat "%&preamble\n" 
                      (replace-regexp-in-string 
                       "\\\\documentclass\\[[^]]+]{[a-z 0-9]+}\n" "" template)))) ; delete \documentclass
        ;; else just call the function as-is
        (funcall fun contents info))))

(defun tlt-tool-bar-toggle-precompile-button ()
  "Add/remove precompilation button to/from the tool-bar in TeX mode."
  (let ((button (assoc 'precompile TeX-bar-TeX-button-alist)))
                                        ; if the precompiling command is in the tool-bar, remove it
    (if button
        (progn  

          (setq TeX-bar-TeX-button-alist (delete precompile-button TeX-bar-TeX-button-alist))
          (setq TeX-bar-TeX-buttons (delete 'precompile TeX-bar-TeX-buttons))
          (setq TeX-bar-LaTeX-buttons (delete 'precompile TeX-bar-LaTeX-buttons))
          (LaTeX-install-toolbar)
          (TeX-install-toolbar))

                                        ; if it is not, add it
      (when tlt-precompile-button-tb
        (add-to-list 'TeX-bar-TeX-button-alist precompile-button t)) ; t for appending
      (add-to-list 'TeX-bar-TeX-buttons 'precompile t)
      (add-to-list 'TeX-bar-LaTeX-buttons 'precompile t)
      (LaTeX-install-toolbar)
      (TeX-install-toolbar)))

  nil)

(defun tlt-tex-install-precompiling ()
  (advice-add 'TeX-run-command :around #'directory-wrapper)
  (advice-add 'org-beamer-template :around #'tlt-preamble-template)
  (advice-add 'org-latex-template  :around #'tlt-preamble-template)
  (add-hook 'tool-bar-mode-hook #'tlt-tool-bar-toggle-precompile-button)
  (add-to-list 'TeX-command-list precompile-list-item t))

(defun tlt-tex-uninstall-precompiling ()
  (advice-remove 'TeX-run-command #'directory-wrapper)
  (advice-remove 'org-beamer-template #'tlt-preamble-template)
  (advice-remove 'org-latex-template  #'tlt-preamble-template)
  (remove-hook 'tool-bar-mode-hook #'tlt-tool-bar-toggle-precompile-button)
  (setq TeX-command-list (delete precompile-list-item TeX-command-list)))

(provide 'tlt-tex-precompile)

;; tlt-tex-precompile.el ends here ;;
