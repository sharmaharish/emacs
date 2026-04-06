;;; hs-lisp.el --- Common Lisp + Emacs Lisp setup (SBCL + SLIME)

;;; Commentary:
;; Modern Lisp setup using:
;; - SBCL (runtime)
;; - SLIME (REPL + dev)
;; - Paredit (structural editing)
;; - Eldoc (documentation)
;; - Corfu/Cape (completion via CAPF)

;;; Code:

;; ------------------------
;; SLIME (Common Lisp)
;; ------------------------
(use-package slime
  :ensure t
  :defer t
  :init
  ;; Use SBCL
  (setq inferior-lisp-program "sbcl")
  :config
  ;; Full SLIME experience
  (slime-setup '(slime-fancy))

  ;; REPL history
  (setq slime-repl-history-file "~/.emacs.d/slime-history.eld"
        slime-repl-history-size 1000))

;; ------------------------
;; Common Lisp Mode Setup
;; ------------------------
(defun my/lisp-mode-setup ()
  "Setup Common Lisp development environment."
  (slime-mode 1)

  ;; Completion (works with Corfu)
  (setq-local completion-at-point-functions
              '(slime-complete-symbol
                completion-at-point)))

(add-hook 'lisp-mode-hook #'my/lisp-mode-setup)

;; ------------------------
;; Paredit (Structural Editing)
;; ------------------------
(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          ielm-mode
          eval-expression-minibuffer-setup
          slime-repl-mode) . paredit-mode))

;; ------------------------
;; Eldoc (Documentation)
;; ------------------------
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'eldoc-mode)

;; ------------------------
;; Visual Improvements
;; ------------------------
(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode scheme-mode) . rainbow-delimiters-mode))

(show-paren-mode 1)

;; ------------------------
;; Keybindings
;; ------------------------
(global-set-key (kbd "C-c s") #'slime)                 ;; start REPL
(global-set-key (kbd "C-c C-z") #'slime-switch-to-output-buffer)

(with-eval-after-load 'slime
  ;; Navigation
  (define-key slime-mode-map (kbd "M-.") #'slime-edit-definition)
  (define-key slime-mode-map (kbd "M-,") #'slime-pop-find-definition)

  ;; Evaluation
  (define-key slime-mode-map (kbd "C-c C-c") #'slime-eval-defun)
  (define-key slime-mode-map (kbd "C-x C-e") #'slime-eval-last-expression))

;; ------------------------
;; Common Lisp HyperSpec
;; ------------------------
(setq common-lisp-hyperspec-root
      "http://www.lispworks.com/documentation/HyperSpec/")

(provide 'hs-lisp)
;;; hs-lisp.el ends here
