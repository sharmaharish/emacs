;; -----------------------------------------------------------------------------
;; KEY BINDINGS
;; -----------------------------------------------------------------------------
;;
(global-set-key (kbd "<f4>") 'query-replace)
(global-set-key (kbd "<f5>") 'calendar)

(global-set-key (kbd "M-s") 'sort-lines)
(global-set-key (kbd "M-g") 'goto-line)

;; Make Emacs use "newline-and-indent" when you hit the Enter key so
;; that you don't need to keep using TAB to align yourself when coding.
(global-set-key (kbd "RET") 'newline-and-indent)

;; ;; capitalize current word (for example, C constants)
;; (global-set-key (kbd "M-u") '(lambda ()
;;                                (interactive)
;;                                (backward-word 1)
;;                                (upcase-word 1)))

(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "M-<down>") 'forward-paragraph)
(global-set-key (kbd "<backtab>") 'toggle-source-header)

(global-set-key (kbd "C-x C-b") 'my-list-buffers)
(global-set-key (kbd "C-x C-x") 'shell)

(define-key minibuffer-local-map (kbd "M-RET") 'file-cache-minibuffer-complete)

;; -----------------------------------------------------------------------------
;; COMMAND ALIASES
;; -----------------------------------------------------------------------------
;;
(defalias 'rb 'rename-buffer)
(defalias 'sh 'shell)

;; -----------------------------------------------------------------------------
;; ADD HOOKS
;; -----------------------------------------------------------------------------
;;
;; deletes all trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; gud-mode (debugging with gdb)
(add-hook 'gud-mode-hook
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
;; org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-RET") 'org-insert-todo-heading)))

(add-hook 'java-mode-hook
          (lambda ()
            (setq compilation-read-command nil)
            (local-set-key (kbd "<f5>") 'compile)
            (set (make-local-variable 'compile-command)
                 (concat (prefix-home "scripts/runp ") buffer-file-name))))

(add-hook 'go-mode-hook
          (lambda ()
            (setq compilation-read-command nil)
            (local-set-key (kbd "<f5>") 'compile)
            (set (make-local-variable 'compile-command)
                 (concat (prefix-home "scripts/runp ") buffer-file-name))))

;; set gtags-mode for c/c++ files
(add-hook 'c-mode-hook
      (lambda ()
        ;;(gtags-mode 1))
        (setq compilation-read-command nil)
        (local-set-key (kbd "<f5>") 'compile)
        (set (make-local-variable 'compile-command)
             (concat (prefix-home "scripts/runp ") buffer-file-name))))

(add-hook 'c++-mode-hook
      (lambda ()
        ;;(gtags-mode 1))
        (setq compilation-read-command nil)
        (local-set-key (kbd "<f5>") 'compile)
        (set (make-local-variable 'compile-command)
             (concat (prefix-home "scripts/runp ") buffer-file-name))))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'compile)
            (set (make-local-variable 'compile-command)
                 (concat (prefix-home "scripts/runp ") buffer-file-name))))

;; -----------------------------------------------------------------------------
;; SET/RESET ATTRIBUTES
;; -----------------------------------------------------------------------------
;;
(fset 'yes-or-no-p 'y-or-n-p)
;; (tool-bar-mode -1)
;; (scroll-bar-mode nil)
;; (menu-bar-mode -1)
(follow-mode t)

(transient-mark-mode t)
(setq-default abbrev-mode t)

(global-font-lock-mode t)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq display-time-day-and-date t)
(display-time)

;; (set-foreground-color "grey")
;; (set-background-color "grey10")
;; (set-cursor-color "grey")
;; (setq frame-title-format "%b")
