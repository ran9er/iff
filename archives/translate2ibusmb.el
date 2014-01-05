(defun translate-mb-read (in)
  (with-temp-buffer
    (let ((enable-local-variables nil))
      (insert-file-contents in)
      (mapcar (lambda (x)
                (split-string x "\s" t))
              (split-string
               (buffer-string) "\n" t)))))

(defun translat-mb-proc1 (lst)
  (with-temp-buffer
    (let ((i "") (w ""))
      (mapc (lambda (x)
              (let* ((a (car x))
                     (b (cdr x))
                     (c (length b)))
                (mapc
                 (lambda (y)
                   (if (eq (length y) 1)
                       (setq i (concat i  a "\t" y "\t" (number-to-string (* c 100)) "\n")
                             c (1- c))
                     (setq w (concat w  a "\t" y "\n"))))
                 b)))
            lst)
      (list i w))))

(defun translat-mb-proc (lst)
  (with-temp-buffer
    (let ((i ""))
      (mapc (lambda (x)
              (let* ((a (car x))
                     (b (cdr x))
                     (c (length b)))
                (mapc
                 (lambda (y)
                   (setq i (concat i  a "\t" y "\t" (number-to-string (* c 100)) "\n")
                         c (1- c)))
                 b)))
            lst)
      i)))

(setq xxxx (translat-mb-proc (translate-mb-read "~/document/wb.txt")))
#(insert xxxx)
