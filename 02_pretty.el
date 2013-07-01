;; * add-watchwords
(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

;; * pretty symbol
(defvar *unicode-symbol*
  (mkht
   left-corner-bracket #X300C
   right-corner-bracket #X300D
   open-parenthesis #XFF08
   close-parenthesis #XFF09
   vertical-bar #@7:#X2502 #@7:#X2503 #XFF5C
   double-vertical-bar #X2016
   backslash #X2198 #@7:#XFF3C #@7:#X2572
   left-arrow 8592
   up-arrow 8593
   right-arrow 8594
   down-arrow 8595
   double-vertical-bar #X2551
   equal #X003d
   not-equal #X2260
   identical #X2261
   not-identical #X2262
   less-than #X003c
   greater-than #X003e
   less-than-or-equal-to #X2264
   greater-than-or-equal-to #X2265
   logical-and #X2227
   logical-or #X2228
   logical-neg #X00AC
   'nil #X2205
   dagger #X2020
   double-dagger #X2021
   horizontal-ellipsis #X2026
   reference-mark #X203B
   double-exclamation #X203C
   prime #X2032
   double-prime #X2033
   for-all #X2200
   there-exists #X2203
   element-of #X2208
   square-root #X221A
   squared #X00B2
   cubed #X00B3
   lambda #X03BB
   ;; lambda [?L (11 . 7) ?M (10 . 1) ?a (11 . 9) ?b (11 . 10) ?d (11 . 1) ?a]
   ;; [?L (11 . 7) ?a (11 . 9) ?M (11 . 9) ?b (11 . 10) ?d (11 . 1) ?a]
   alpha #X03B1
   beta #X03B2
   gamma #X03B3
   delta #X03B4
   ))

(defun substitute-patterns-with-unicode (patterns)
  ""
  (mapcar
   (lambda (x)
     (font-lock-add-keywords
      nil `((,(car x)
             (0 (progn
                  (compose-region (match-beginning 1) (match-end 1)
                                  ;; ,(decode-char 'ucs (gethash (cdr x) *unicode-symbol*))
                                  ,(gethash (cdr x) *unicode-symbol*)
                                  'decompose-region)
                  nil))))))
   patterns))

;; ** lisp symbol
(defun lisp-symbol ()
  (interactive)
  (substitute-patterns-with-unicode
   (to-alist '("(?\\(lambda\\>\\)" lambda
               ;; "\\<\\(lambda\\)\\>" lambda
               "\\(;;\\ \\)" reference-mark
               "\\((elf\\ \\)" element-of
               "\\(\\\\\\\\(\\)" left-corner-bracket
               "\\(\\\\\\\\)\\)" right-corner-bracket
               "\\(\\\\\\\\|\\)" vertical-bar
               "\\(\\\\\\\\\\)" backslash
               ;; "\\(<-\\)" left-arrow
               ;; "\\(->\\)" right-arrow
               ;; "\\(==\\)" identical
               ;; "\\(/=\\)" not-identical
               ;; "\\(>=\\)" greater-than-or-equal-to
               ;; "\\(<=\\)" less-than-or-equal-to
               ;; "\\(\\.\\.\\)" horizontal-ellipsis
               ;; "\\(()\\)" 'nil
               ;; "\\(!!\\)" double-exclamation
               ))))

;; * lisp block comment
(defun lisp-block-comment ()
  (interactive)
  (font-lock-add-keywords
   nil `(("\\(#@[0-9]+:\\)"
          1 font-lock-warning-face t))))
