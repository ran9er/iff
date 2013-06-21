;; -*- encoding: utf-8-unix; -*-
;; don't need load-once, because eshell-load-hook load only once
;;;###autoload
(autoload 'eshell "eshell" "" t)

;; * bash-completion
;+++++++++++++++++++++++++++++++++++++++
(when nil
;+++++++++++++++++++++++++++++++++++++++
(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
  'bash-completion-dynamic-complete)
;+++++++++++++++++++++++++++++++++++++++
)
;+++++++++++++++++++++++++++++++++++++++

;; * em-smart
;+++++++++++++++++++++++++++++++++++++++
(when nil
;+++++++++++++++++++++++++++++++++++++++
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'after
      eshell-review-quick-commands t
      eshell-smart-displayed t
      eshell-smart-space-goes-to-end t)
;+++++++++++++++++++++++++++++++++++++++
)
;+++++++++++++++++++++++++++++++++++++++

;; * hook
;; C-u M-x eshell 多开
(add-hook 'eshell-mode-hook (lambda()
           (outline-minor-mode 1)
           (eldoc-mode)
;           (lisp-symbol)
           (skeleton-pair-alist-update)
           ;; (enable-theme 'eshell)
           (eshell-scroll-conservatively)
           (setq
                 pcomplete-cycle-completions   nil
;                 pcomplete-cycle-cutoff-length 4
                 outline-regexp "^[^#$\n]* [#$>]+ "
                 eshell-scroll-to-bottom-on-output t
                 eshell-scroll-show-maximum-output t)
           (add-to-list 'eshell-output-filter-functions
                        'eshell-postoutput-scroll-to-bottom)
            (def-key-s eshell-mode-map
              ;; "C-p"   'eshell-previous-matching-input-from-input
              ;; "C-n"   'eshell-next-matching-input-from-input
              ;; "M-p"   'previous-line
              ;; "M-n"   'next-line
              "<up>"    'eshell-previous-matching-input-from-input
              "<down>"  'eshell-next-matching-input-from-input
              "C-9"     (lambda(&optional x)(interactive "P")(outside "()" 1 " " x))
              "C-8"     'down-list
              "C-7"     '(lambda nil (interactive)(up-list -1))
              )
;          (buffer-face-set 'eshell-custom-face)
))

;; * setting
(setq
      eshell-save-history-on-exit   t
      eshell-history-size           512
      eshell-hist-ignoredups        t
      eshell-cmpl-ignore-case       t
      eshell-cp-interactive-query   t
      eshell-ln-interactive-query   t
      eshell-mv-interactive-query   t
      eshell-rm-interactive-query   t
      eshell-mv-overwrite-files     nil
      ;;  aliases file 中不能有多余的空行，否则报正则表达式错误
      eshell-aliases-file       (expand-file-name "_eshell_/eshell-alias" iff-source)
      eshell-highlight-prompt   t
      ;; 提示符设置，两项必须对应起来，否则报 read-only 且不能补全
      eshell-prompt-regexp      "^[^#$\n]* [#$>]+ "
      eshell-prompt-function    (lambda nil
                                  (concat
                                   (abbreviate-file-name
                                    (eshell/pwd))
                                   (if
                                       (=
                                        (user-uid)
                                        0)
                                       " # " " $ ")))
)

(defun eshell-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (mapc (lambda(x)(set (make-local-variable (car x))(cdr x)))
        (to-alist '(scroll-margin            0
                    scroll-conservatively    10
                    scroll-step              1))))

;; * exntension
(mapcar
 'require
 '(eshell-ls
   eshell-user-key
   eshell-user-func
   eshell-cmpl
   eshell-bmk))

;; * face
;(make-face 'eshell-custom-face)
;(set-face-attribute 'eshell-custom-face nil :font "宋体-10")

;; * last command timer
(add-hook 'eshell-load-hook
          (lambda()(setq last-command-start-time (float-time))))
(add-hook 'eshell-pre-command-hook
          (lambda()(setq last-command-start-time (float-time))))
(add-hook 'eshell-before-prompt-hook
          (lambda()
              (message "%s ==> spend %g seconds"
                       (cond
                        ((not eshell-last-command-name) "GO!")
                        ((string-match "^#<" eshell-last-command-name)
                         (substring eshell-last-command-name 2 -1))
                        (t eshell-last-command-name))
                       (- (float-time) last-command-start-time))))

;; * ac-mode
;+++++++++++++++++++++++++++++++++++++++
(when nil
;+++++++++++++++++++++++++++++++++++++++
(defvar ac-source-eshell-pcomplete
  '((candidates . (pcomplete-completions))))
(defun ac-complete-eshell-pcomplete ()
  (interactive)
  (auto-complete '(ac-source-eshell-pcomplete)))
;; (ac-define-source eshell-pcomplete
;;   '((candidates . (pcomplete-completions))
;;     (cache)))
;; (push 'ac-complete-eshell-pcomplete ac-trigger-commands)

(add-to-list 'ac-modes 'eshell-mode)
(setq ac-sources '(;; ac-source-semantic
                   ;; ac-source-yasnippet
                   ac-source-eshell-pcomplete
                   ;; ac-source-files-in-current-dir
                   ;; ac-source-filename
                   ;; ac-source-abbrev
                   ;; ac-source-words-in-buffer
                   ;; ac-source-words-in-all-buffer
                   ;; ac-source-symbols
                   ;; ac-source-imenu
))
;+++++++++++++++++++++++++++++++++++++++
)
;+++++++++++++++++++++++++++++++++++++++
