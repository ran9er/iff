;; -*- encoding: utf-8-unix; -*-
;; File-name:    <file.el>

;;;###autoload
(defun read-lines (filePath)
  "Return a list of lines in FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string
     (buffer-string) "\n" t)))

;;;###autoload
(defun find-temp (&optional suffix)
  (interactive "sExtension: ")
  (let ((suf (concat "."
                     (if (and suffix (null (string= suffix "")))
                         suffix "markdown"))))
    (find-file
     (concat
      (make-temp-name
       (expand-file-name
        (format-time-string "%Y%m%d%H%M%S-" (current-time))
        work-dir))
      suf))
    (run-hooks 'find-temp-hook)))

;;;###autoload
(defun write-temp (filename &optional confirm)
  (interactive
   (list (if buffer-file-name
             (read-file-name "Write file: "
                             nil nil nil nil)
           (read-file-name "Write file: " default-directory
                           (expand-file-name
                            (file-name-nondirectory (buffer-name))
                            default-directory)
                           nil nil))
         (not current-prefix-arg)))
  (let ((fnm buffer-file-name))
    (write-file filename confirm)
    (if (file-exists-p fnm)
        (delete-file fnm))))

(add-hook 'find-temp-hook (lambda ()
                            (yank)))
