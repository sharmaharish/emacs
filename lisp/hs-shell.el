
(defun track-shell-directory/procfs ()
  (shell-dirtrack-mode 0)
  (add-hook 'comint-preoutput-filter-functions
            (lambda (str)
              (prog1 str
                (when (string-match comint-prompt-regexp str)
                  (cd (file-symlink-p
                       (format "/proc/%s/cwd" (process-id
                                               (get-buffer-process
                                                (current-buffer)))))))))
            nil t))

;; changes the file coding system to unix before saving
;; (add-hook 'before-save-hook 'set-unix-file-coding-system)

;; will truncate shell buffer to comint-buffer-maximum-size.
;; (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; will disalllow passwords to be shown in clear text (this is useful,
;; for example, if you use the shell and then, login/telnet/ftp/scp etc. to other machines).
;; (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; will remove ctrl-m from shell output.
;; (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; shell mode hooks
(add-hook 'shell-mode-hook
          '(lambda ()
             (local-set-key [home] ; move to beginning of line, after prompt
                            'comint-bol)
             (local-set-key [up] ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
             (local-set-key [down] ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))))
;; Fix junk characters in shell mode
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-hook 'shell-mode-hook 'track-shell-directory/procfs)

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))
