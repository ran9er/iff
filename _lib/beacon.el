;;;###autoload
(defun swap-point()
  (interactive)
  (if (or (null (boundp '*last-point*)) (null *last-point*))
      (progn (make-local-variable '*last-point*)
             (setq *last-point* (cons (point) (point))))
    (let ((p (point)))
      (if (eq p (cdr *last-point*))
          (progn (goto-char (car *last-point*))
                 (setq *last-point* (cons (cdr *last-point*)(car *last-point*))))
        (goto-char (cdr *last-point*))
        (setq *last-point* (cons p (cdr *last-point*)))))))

;;;###autoload
(defun beacon ()
  (interactive)
  (let ((k (where-is-internal 'beacon-jump)))
    (message (concat (mapconcat 'key-description k " , ")
                     (if k " or ")
                     "C-M-c to jump back.")))
  (save-excursion
    (save-window-excursion
      (catch (recursion-depth)
        (recursive-edit)))))

(defun beacon-jump (&optional n)
  (interactive "p")
  (let* ((x (recursion-depth))
         (i (if (> (or n 1) x)
                x n))
         (tag (- x i)))
    (condition-case msg
        (throw tag t)
      (error
       (if (equal msg '(no-catch 0 t))
           (message "no exit"))))))
