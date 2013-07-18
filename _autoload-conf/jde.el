(defvar jdee-path (expand-file-name "jdee/lisp" exts-dir))
(add-to-list 'load-path jdee-path)
(load (expand-file-name "jde" jdee-path))
(setq jde-check-version-flag nil)
(define-obsolete-function-alias 'make-local-hook 'ignore "21.1")
(unless (fboundp 'semantic-format-prototype-tag-java-mode)
  (defalias 'semantic-format-prototype-tag-java-mode 'semantic-format-tag-prototype-java-mode))
(require 'hippie-exp)

;; JAVA_HOME
(unless (getenv "JAVA_HOME")
  (let ((java (if (eq system-type 'windows-nt)
                  "C:\\Program Files\\Java")))
    (setenv "JAVA_HOME" (car (nreverse (directory-files java t "jdk" 'string-lessp))))))
;; JRE_HOME
(setenv "JRE_HOME" (expand-file-name "jre" (getenv "JAVA_HOME")))
;; CLASSPATH
(when nil
  (mapc
   (lambda(x)
     (add-environment "CLASSPATH" (expand-file-name "lib" (getenv x))))
   '("JAVA_HOME" "JRE_HOME")))
(add-environment "CLASSPATH" ".")
;; JAVA_TOOL_OPTIONS
(setenv "JAVA_TOOL_OPTIONS" "-Dfile.encoding=utf-8 -Duser.language=cn -Duser.country=ZH")
;; JDK
(let* ((ver "1.7")
       (jdk (getenv "JAVA_HOME")))
  ;; (custom-set-variables `(jde-jdk-registry (quote ((,ver . ,jdk)))))
  (setq 
   ;; jde-sourcepath '( "/Users/ldangelo/Development" )
   jde-db-option-connect-socket '(nil "28380")
   jde-jdk-registry `((,ver . ,jdk))
   jde-jdk `(,ver)
   ;; jde-doc-dir "~/d/jdk-6-doc/"
   jde-web-browser "firefox"))

(push 'jde-mode ac-modes)

(add-hook 'jde-mode-hook (lambda () (push 'ac-source-semantic ac-sources)))

(defun ac-semantic-candidate (prefix)
  (if (memq major-mode
            '(c-mode c++-mode jde-mode java-mode))
      (mapcar 'semantic-tag-name
              (ignore-errors
                (or (semantic-ia-get-completions
                     (semantic-analyze-current-context) (point))
                    (senator-find-tag-for-completion (regexp-quote prefix)))))))

(push
 '(candidates . (lambda () (all-completions ac-prefix (ac-semantic-candidate ac-prefix))))
 ac-source-semantic)

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-decoration-mode
                                  global-semantic-highlight-func-mode
                                  global-semantic-stickyfunc-mode
                                  global-semantic-mru-bookmark-mode))

;; (add-to-list 'load-path (expand-file-name "~/Documents/elisp/jdibug-0.2"))
(setq semantic-load-turn-everything-on t)
(semantic-mode 1)
(require 'semantic/senator)
(require 'semantic)
(require 'semantic/ia)
(require 'semantic/wisent)
(require 'semantic/wisent/java-tags)

;; Use the full Java 1.5 grammer to parse Java
(autoload 'wisent-java-default-setup "wisent" "Hook run to setup Semantic in 'java-mode'." nil nil)

(setq jde-auto-parse-enable nil)
(setq jde-enable-senator nil)
(load "jde-autoload")

;; load jde-testng
(require 'jde-testng)

;; load jde-maven
(require 'jde-maven2)

(require 'jdibug)

;; If you want Emacs to defer loading the JDE until you open a 
;; Java file, edit the following line
(setq defer-loading-jde nil)
;; to read:
;;
;;  (setq defer-loading-jde t)
;;

;; (if defer-loading-jde
;;     (progn
;;       (autoload 'jde-mode "jde" "JDE mode." t)
;;       (setq auto-mode-alist
;; 	    (append
;; 	     '(("\\.java\\'" . jde-mode))
;; 	     auto-mode-alist)))
;;   (require 'jde))

;; Include the following only if you want to run
;; bash as your shell.

;; Setup Emacs to run bash as its primary shell.
;; (setq shell-file-name "bash")
;; (setq shell-command-switch "-c")
;; (setq explicit-shell-file-name shell-file-name)
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-sh-args '("-login" "-i"))
;; (if (boundp 'w32-quote-process-args)
;;   (setq w32-quote-process-args ?\")) ;; Include only for MS Windows.


;; Location of you emacs directory
(setq my-emacs-dir (concat (getenv "HOME") "/.emacs.d/tmp/emacs-jde"))

;; save all the semantic.cache files to one place
(when (locate-library "semantic")
  (let ((semcach (concat my-emacs-dir "/semantic-cache")))
    (unless (file-directory-p semcach)
      (make-directory semcach))
    (setq semanticdb-default-save-directory semcach)))

(define-key jde-mode-map [f8]   'jdibug-step-over) 
(define-key jde-mode-map [M-f8] 'jdibug-step-into) 
(define-key jde-mode-map [f7]   'jdibug-step-out) 
(define-key jde-mode-map [M-f7] 'jdibug-resume)

(require 'flymake)

(defun skip-cleanup())

;; function does not exist in emacs 23.2
(defun semantic-parse())

(defun flymake-java-ecj-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'jde-ecj-create-temp-file))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    ;; Change your ecj.jar location here
    (list "java" (list "-jar" "/Users/ldangelo/Development/ecj.jar" "-Xemacs" "-d" "/dev/null"
                       "-source" "1.5" "-target" "1.5" "-proceedOnError"
                       "-classpath"
                       (jde-build-classpath jde-global-classpath) local-file))))
 
(defun flymake-java-ecj-cleanup ()
  "Cleanup after `flymake-java-ecj-init' -- delete temp file and dirs."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (flymake-safe-delete-directory (file-name-directory flymake-temp-source-file-name))))

(defun jde-ecj-create-temp-file (file-name prefix)
  "Create the file FILE-NAME in a unique directory in the temp directory."
  (file-truename (expand-file-name (file-name-nondirectory file-name)
                                   (expand-file-name  (int-to-string (random)) (flymake-get-temp-dir)))))
 
(push '(".+\\.java$" flymake-java-ecj-init flymake-java-ecj-cleanup) flymake-allowed-file-name-masks)
 
(push '("\\(.*?\\):\\([0-9]+\\): error: \\(.*?\\)\n" 1 2 nil 2 3 (6 compilation-error-face)) compilation-error-regexp-alist)
 
(push '("\\(.*?\\):\\([0-9]+\\): warning: \\(.*?\\)\n" 1 2 nil 1 3 (6 compilation-warning-face)) compilation-error-regexp-alist)

;; Sets the basic indentation for Java source files
;; to two spaces.
(defun my-jde-mode-hook ()
  "Hook for running java file..."
  (message " Loading my-jde-mode-hook...")
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'case-label '+)
  ;; (auto-complete-mode)
  ;; (ac-settings-4-java)
  (wisent-java-default-setup)
  (flymake-mode)
  (setq 
   indent-tabs-mode nil
   tab-width 4
   c-basic-offset 2
   tempo-interactive t
   ))

(add-hook 'jde-mode-hook 'my-jde-mode-hook)

;;;###autoload
(autoload 'jde-mode (expand-file-name "jde" jdee-path) "" t)
