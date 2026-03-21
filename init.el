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

;; (require 'hs-default)
;; (require 'hs-shell)
;; (require 'hs-lisp)
;; (require 'hs-python)

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
  (completion-category-defaults nil))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)))
;;         ("M-x" . consult-M-x)))

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

