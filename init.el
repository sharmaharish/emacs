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

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  ;; Terminal-specific settings - these ARE the correct variables
  (corfu-popupinfo-mode nil)  ; Disable popup info in terminal
  (corfu-min-width 25)
  (corfu-max-width 80)
  (corfu-count 10)
  ;; Make Corfu work better in terminal
  (corfu--frame nil)  ; Don't use child frames in terminal
  :init
  (global-corfu-mode)
  ;; Fix for terminal: ensure we use overlay popups, not child frames
  (unless (display-graphic-p)
    (setq corfu--frame nil
          corfu-popupinfo-mode nil))
  :config
  ;; Protect against the nil position error
  (advice-add 'corfu--popup-show :around
              (lambda (orig &rest args)
                (condition-case nil
                    (apply orig args)
                  (error (message "Corfu popup error suppressed") nil)))))

;; (use-package corfu
;;   :custom
;;   (corfu-auto t)
;;   (corfu-auto-delay 0.1)
;;   (corfu-auto-prefix 2)
;;   ;; Minimal terminal settings
;;   (corfu-popupinfo-mode nil)
;;   (corfu-terminal-mode +1)
;;   :config
;;   ;; Override the position calculation function
;;   (advice-add 'corfu--popup-show :around
;;               (lambda (orig &rest args)
;;                 (condition-case nil
;;                     (apply orig args)
;;                   (error nil))))
;;   :init
;;   (global-corfu-mode))

;; (use-package corfu
;;   :custom
;;   (corfu-auto t)           ; Enable auto completion
;;   (corfu-auto-delay 0.2)   ; Small delay for better performance
;;   (corfu-auto-prefix 2)    ; Start after 2 characters
;;   (corfu-popupinfo-mode t) ; Show documentation popup (optional)
;;   :init
;;   (global-corfu-mode))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

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

