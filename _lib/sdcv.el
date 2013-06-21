(if (eq system-type 'windows-nt)
    (progn 
      (add-exec-path 
       (expand-file-name "../other/sdcv/" exec-directory))
      (setq 
       sdcv-cmd
       (format 
        "sdcv --data-dir %s" 
        (expand-file-name "../other/sdcv/dict/" exec-directory)))))

;;;###autoload
(defun sdcv-to-buffer ()
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                  (current-word nil t))))
    (setq word (read-string (format "查字典 (默认 %s): " word)
                            nil nil word))
    (set-buffer (get-buffer-create "*sdcv*"))
;    (set-buffer-file-coding-system cp936)  ;;
    (if (fboundp 'custom-buffer-face-mode)
        (custom-buffer-face-mode))
    (buffer-disable-undo)
    (erase-buffer)
    ; 在没有创建 *sdcv* windows 之前检查是否有分屏(是否为一个window)
    ; 缺憾就是不能自动开出一个小屏幕，自己注销
    (if (null (cdr (window-list)))
        (setq onewindow t)
      (setq onewindow nil))
    (let ((process (start-process-shell-command "sdcv" "*sdcv*" sdcv-cmd word)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (unless (string= (buffer-name) "*sdcv*")
             (setq kid-sdcv-window-configuration (current-window-configuration))
             (switch-to-buffer-other-window "*sdcv*")
             (local-set-key (kbd "d") 'sdcv-to-buffer)
             (local-set-key (kbd "n") 'next-line)
             (local-set-key (kbd "j") 'next-line)
             (local-set-key (kbd "p") 'previous-line)
             (local-set-key (kbd "k") 'previous-line)
             (local-set-key (kbd "SPC") 'scroll-up)
             (local-set-key (kbd "DEL") 'scroll-down)
             (local-set-key (kbd "q") (lambda ()
                                        (interactive)
                                        (if (eq onewindow t)
                                            (delete-window)
                                          (progn (bury-buffer) (other-window 1))))))
           (goto-char (point-min))))))))
