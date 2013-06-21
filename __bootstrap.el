(defvar initial-framework-for-emacs nil)

(defvar iff--find-path
  (lambda (regexp)
    (let* ((this-file (file-name-nondirectory load-file-name))
           (this-dir (file-name-directory load-file-name))
           base-dir)
      (cond
       ;; when specify iff-source outside, and load this file
       ((boundp 'iff-source) nil)
       ;; when this file's name is /home/.../.emacs or /.../emacs/.../site-start.el
       ((member load-file-name
                (mapcar 'expand-file-name
                        (list "~/.emacs" (locate-library site-run-file))))
        (setq base-dir
              (apply 'expand-file-name
                     (cond
                      ((eq system-type 'windows-nt)
                       `(".." ,exec-directory))
                      (t
                       `("~")))))
        ((lambda (x) (file-name-as-directory
                 ;; iff-source is the newest directory with "init" and "emacs" in it's name
                 ;; or directory where this file is located (iff-source is $HOME or site-lisp)
                 (or (car x) this-dir)))
         (let (result)
           (mapc
            (lambda (f) (if (file-directory-p f)(setq result (append result (list f)))))
            (directory-files base-dir t regexp 'file-newer-than-file-p))
           (setq iff-source-candidates result))))
       ;; when this file's name is not .emacs or site-start.el, for example as bootstrap.el
       ;; load this file in emacs init file : (load "...../bootstrap.el")
       (t this-dir)))))

(defvar iff--find-files
  (lambda (dir exp)
    (mapcar
     (lambda (f) (file-name-sans-extension f))
     (directory-files dir t exp))))

(defvar iff-length nil)

(defvar iff-source-candidates nil)
(defvar iff-branch
  '((lib-dir   .   "_lib/")
    (lib-df    .   "_loaddefs")
    (ext-dir   .   "_extensions_")
    (eal-dir   .   "_eval-after-load/")
    (alc-dir   .   "_autoload-conf/")
    (wk-dir    .   "sandbox/")))

(defvar iff-source (funcall iff--find-path "iff\\|init.*el\\|init.*emacs\\|emacs.*init"))
(defvar iff-pre-init-files
  (funcall iff--find-files iff-source "^__.*\\.el\\'"))
(defvar iff-init-files
  (funcall iff--find-files iff-source "^[^_].*\\.el\\'"))

(defvar iff--startup-hook
  '(lambda ()
     (mapc
      (lambda(x) (plist-put (car iff-length) (car x)(cdr x)))
      (list
       (cons 'emacs
             (- (float-time after-init-time) (float-time before-init-time)))
       (cons 'other
             (- (float-time) (float-time after-init-time)))))
     (message "load %d init file, spend %g seconds; startup spend %g seconds"
              (- (length iff-length) 1)
              (plist-get (car iff-length) 'init)
              (+
               (plist-get (car iff-length) 'emacs)
               (plist-get (car iff-length) 'other)))))

(defvar iff--message
  (lambda (msg)
    (message (concat "=======>" msg))))

(defvar iff--check-directory
  (lambda (p base &optional dir-p)
    (let ((f (expand-file-name p base)))
      (unless (file-exists-p f)
        (if dir-p
            (progn (make-directory f)
                   (message (concat "New dir " f)))
          (progn (find-file f)
                 (message (concat "New file " f))))))))

(defvar iff--load
  (lambda (lst &optional var)
    (let* ((var (or var 'iff-length))
           tm)
      (funcall iff--message (format "Load %s" lst))
      (mapc
       (lambda (f)
         (setq tm (float-time))
         (load f)
         (set var
              (cons
               (cons
                (file-name-nondirectory f)
                (- (float-time) tm))
               (eval var))))
       (eval lst)))))

(defvar iff--autoload
  (lambda (dir &optional var)
    ;; dir full-path
    ;; var 'iff-length
    (let* ((d (expand-file-name dir iff-source))
           (var (or var 'iff-length))
           (load-file-name (expand-file-name (make-temp-name "") d)))
      (funcall iff--message (format "Load %s" dir))
      (funcall iff--check-directory dir iff-source t)
      (set var
           (cons
            (cons
             (format "gen autoload for %s" dir)
             (lazily d))
            (eval var))))))

(defvar iff--eval-after-load
  (lambda (path &optional var)
    (let ((tmp (float-time))
          (var (or var 'iff-length))
          (dir (cdr (assoc path iff-branch))))
      (funcall iff--message (format "Eval-after-load %s" name))
      (defvar iff-feature-file-hash (make-hash-table :test 'equal :size 20))
      (funcall iff--check-directory dir iff-source t)
      (mapc
       (lambda (x)
         (puthash
          (intern (file-name-sans-extension (file-name-nondirectory x))) x
          iff-feature-file-hash))
       (directory-files (expand-file-name dir iff-source) t "\\.el\\'"))
      (maphash
       (lambda (x y)
         (eval-after-load x `(load ,y))
         (message (format "eval-after-load %s" y)))
       iff-feature-file-hash)
      (set var
           (cons
            (cons "gen eval-after-load"
                  (- (float-time) tmp))
            (eval var))))))

(defvar iff-
  (lambda (name &optional value)
    (setq value (eval name))
    (when initial-framework-for-emacs
      (throw 'quit "have been loaded"))
    (setq initial-framework-for-emacs t)
    (funcall iff--message (format "Find %s" name))
    (message (format "%s is %s" name value))
    (setq iff-branch
          (mapcar
           (lambda(x)
             (cons
              (car x)
              (expand-file-name (cdr x) value)))
           iff-branch))
    (when (null (file-exists-p value))
      (throw 'quit (format "can't found %s" name)))

    (funcall iff--load 'iff-pre-init-files)

    (funcall iff--message "Add load-path")
    (mapc
     (lambda (p)
       (if (file-directory-p p)
           (and
            (add-to-list 'load-path p)
            (message (format "Add %s to load-path" p)))))
     (directory-files value t "^_.*_\\'"))

    (funcall iff--autoload (cdr (assoc 'lib-dir iff-branch)))

    (funcall iff--autoload (cdr (assoc 'alc-dir iff-branch)))

    (funcall iff--eval-after-load 'eal-dir)

    (funcall iff--load 'iff-init-files)

    (setq iff-length
          (cons
           (list 'init
                 (apply '+ (mapcar 'cdr iff-length)))
           (reverse iff-length)))

    (add-hook 'emacs-startup-hook iff--startup-hook)))

(defvar iff-status
  (catch 'quit (funcall iff- 'iff-source)))
