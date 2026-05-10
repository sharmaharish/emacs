;;   __    ___         ___ _ __ ___   __ _  ___ ___
;;  |  |__/ __|  __   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  _  \__ \ |__| |  __/ | | | | | (_| | (__\__ \
;;  |__|__|__ /       \___|_| |_| |_|\__,_|\___|___/
;;

;; --------------------------------------------------
;; Package System
;; --------------------------------------------------

(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; --------------------------------------------------
;; use-package bootstrap
;; --------------------------------------------------

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; --------------------------------------------------
;; Load custom file separately
;; --------------------------------------------------

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; --------------------------------------------------
;; Config directory
;; --------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "configs" user-emacs-directory))

(load "hs-shell.el")
(load "hs-lisp.el")
(load "hs-python.el")
(load "hs-org.el")
(load "hs-rust.el")

;; --------------------------------------------------
;; UI / Defaults
;; --------------------------------------------------

(global-hl-line-mode 1)
(column-number-mode 1)

;; --------------------------------------------------
;; Theme
;; --------------------------------------------------

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; --------------------------------------------------
;; Completion (modern stack)
;; --------------------------------------------------

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)))
;;         ("M-x" . consult-M-x)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t)

;; --------------------------------------------------
;; In-buffer completion
;; --------------------------------------------------

(use-package corfu-terminal
  :ensure t)

(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-on-exact-match 'insert)
  :init
  (global-corfu-mode 1)
  :config
  (setq corfu-auto t)
  (setq tab-always-indent 'complete))

(use-package cape
  :after corfu
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history))
 
;; --------------------------------------------------
;; Git
;; --------------------------------------------------

(use-package magit)

;; --------------------------------------------------
;; Project management (built-in)
;; --------------------------------------------------

(use-package project
  :ensure nil)

;; --------------------------------------------------
;; Global Eglot configuration
;; --------------------------------------------------

(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot eglot-ensure)

  :init
  ;; Shutdown server when last managed buffer closes
  (setq eglot-autoshutdown t)

  ;; Reduce noise and memory usage
  (setq eglot-events-buffer-size 0)

  ;; Faster responsiveness
  (setq eglot-send-changes-idle-time 0.2)

  ;; Use xref integration (M-. etc.)
  (setq eglot-extend-to-xref t)

  ;; Ignore noisy/high-frequency features
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider))

  ;; Better CAPF behavior with Corfu
  (setq completion-category-defaults nil)

  :hook
  (;; Languages
   (python-ts-mode . eglot-ensure)
   (python-mode . eglot-ensure)

   (rust-ts-mode . eglot-ensure)

   (c-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)

   (java-mode . eglot-ensure)

   ;; Optional:
   ;; (js-ts-mode . eglot-ensure)
   ;; (typescript-ts-mode . eglot-ensure)
   ;; (go-ts-mode . eglot-ensure)

   ;; Auto-format on save
   (eglot-managed-mode . (lambda ()
                           (add-hook 'before-save-hook
                                     #'eglot-format-buffer
                                     nil t))))

  :config
  ;; ------------------------
  ;; Server definitions
  ;; ------------------------

  ;; Python (Pyright)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "pyright-langserver" "--stdio"))

  ;; Rust
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer")))

  ;; C / C++
  ;; clangd usually auto-detected if installed

  ;; ------------------------
  ;; Keybindings
  ;; ------------------------

  (define-key eglot-mode-map (kbd "C-c g r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c g f") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c g a") #'eglot-code-actions)

  (define-key eglot-mode-map (kbd "C-c g d") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c g i") #'xref-find-implementations)
  (define-key eglot-mode-map (kbd "C-c g t") #'xref-find-type-definition)
  (define-key eglot-mode-map (kbd "C-c g e") #'eglot-find-declaration)

  (define-key eglot-mode-map (kbd "C-c g h") #'eldoc-doc-buffer))

;; --------------------------------------------------
;; Discoverability
;; --------------------------------------------------

(use-package which-key
  :init
  (which-key-mode))

;; --------------------------------------------------
;; Programming defaults
;; --------------------------------------------------

(setq-default
 tab-width 4
 c-basic-offset 4)

(show-paren-mode 1)

;; --------------------------------------------------
;; Convenience keybindings
;; --------------------------------------------------

(global-set-key (kbd "C-c s") 'shell)

(use-package crux
  :ensure t
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-c d" . crux-duplicate-current-line-or-region))

;; --------------------------------------------------
;; macOS shell environment fix
;; --------------------------------------------------

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

