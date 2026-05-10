;;; hs-rust.el --- Modern Rust setup

;;; Commentary:
;; Rust development using:
;; - rust-ts-mode (tree-sitter)
;; - eglot (LSP)
;; - corfu/capf (completion)

;;; Code:

;; ------------------------
;; Rust major mode
;; ------------------------

;; Use rust-ts-mode for .rs files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;; ------------------------
;; Rust setup
;; ------------------------

(defun hs/rust-mode-setup ()
  "Setup Rust development environment."

  ;; Tree-sitter indentation/font-lock
  (when (fboundp 'treesit-parser-create)
    (treesit-parser-create 'rust))

  ;; Start Eglot automatically
  (eglot-ensure)

  ;; Formatting
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)

  ;; Auto format before save
  (add-hook 'before-save-hook #'eglot-format-buffer nil t)

  ;; Use compile command intelligently
  (setq-local compile-command "cargo run"))

(add-hook 'rust-ts-mode-hook #'hs/rust-mode-setup)

;; ------------------------
;; Useful keybindings
;; ------------------------

(defun hs/rust-run ()
  "Run cargo run."
  (interactive)
  (compile "cargo run"))

(defun hs/rust-test ()
  "Run cargo test."
  (interactive)
  (compile "cargo test"))

(defun hs/rust-check ()
  "Run cargo check."
  (interactive)
  (compile "cargo check"))

(defun hs/rust-clippy ()
  "Run cargo clippy."
  (interactive)
  (compile "cargo clippy"))

(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c C-c") #'hs/rust-run)
  (define-key rust-ts-mode-map (kbd "C-c C-t") #'hs/rust-test)
  (define-key rust-ts-mode-map (kbd "C-c C-k") #'hs/rust-check)
  (define-key rust-ts-mode-map (kbd "C-c C-l") #'hs/rust-clippy))

;; ------------------------
;; Cargo.toml support
;; ------------------------

(use-package toml-ts-mode
  :mode "\\.toml\\'")

(provide 'hs-rust)
;;; hs-rust.el ends here
