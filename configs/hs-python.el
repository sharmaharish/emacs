;; Uses python-ts-mode, eglot (Pyright), Corfu+Cape completion, Crux, REPL integration,
;; Python Black formatting, and Flymake for syntax checking.

;; Pre-requisites
;; 1. brew install pyright
;; 2. brew install tree-sitter
;; 3. M-x treesit-install-language-grammar RET python RET - select defaults
;; ------------------------
;; Python tree-sitter mode
;; ------------------------
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

(defun my/python-ts-mode-setup ()
  "Setup Python development environment for python-ts-mode."
  ;; Indentation
  (setq tab-width 4
        indent-tabs-mode nil)
  ;; Line numbers
  (display-line-numbers-mode 1)
  ;; Flymake (syntax checking)
  (flymake-mode 1)
  ;; LSP via eglot
  (eglot-ensure))

(add-hook 'python-ts-mode-hook #'my/python-ts-mode-setup)

;; ------------------------
;; Eglot: Python server (Pyright)
;; ------------------------
(use-package eglot
  :ensure t
  :defer t
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio"))))

;; ------------------------
;; REPL integration
;; ------------------------
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")

(defun my/python-run-buffer ()
  "Send the current buffer to Python REPL."
  (interactive)
  (python-shell-send-buffer))

(defun my/python-run-region-or-line ()
  "Send the current region or line to Python REPL."
  (interactive)
  (if (use-region-p)
      (python-shell-send-region (region-beginning) (region-end))
    (python-shell-send-region (line-beginning-position)
                              (line-end-position))))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'my/python-run-buffer)
            (local-set-key (kbd "C-c C-r") 'my/python-run-region-or-line)
            (local-set-key (kbd "C-c C-z") 'run-python)))

;; ------------------------
;; Python Black formatting
;; ------------------------
(use-package python-black
  :ensure t
  :hook (python-ts-mode . python-black-on-save-mode))

;; ------------------------
;; Optional: Flymake enhancements
;; ------------------------
;; You can also use flycheck if you prefer
(use-package flymake-python-pyflakes
  :ensure t
  :hook (python-ts-mode . flymake-python-pyflakes-load))

(provide 'hs-python)
;;; hs-python.el ends here
