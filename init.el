;;   __    ___         ___ _ __ ___   __ _  ___ ___
;;  |  |__/ __|  __   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  _  \__ \ |__| |  __/ | | | | | (_| | (__\__ \
;;  |__|__|__ /       \___|_| |_| |_|\__,_|\___|___/
;;

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(ido-completing-read+  ;; allow ido usage in as many contexts as possible, see hs-default.el
    smex                  ;; enhances M-x to allow easier execution of commands
    projectile            ;; project navigation
    magit
    zenburn-theme         ;; zenburn theme
    material-theme        ;; material dark theme
    better-defaults       ;; better default options
    ))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun prefix-home (path) (concat user-emacs-directory path))

(add-to-list 'load-path (prefix-home "configs"))

(load "hs-default.el")
(load "hs-shell.el")
(load "hs-lisp.el")
(load "hs-org.el")
(load "hs-cpp.el")
(load "hs-java.el")
(load "hs-python.el")
(load "hs-clojure.el")
(load "hs-packages.el")

(shell)

(load-theme 'zenburn t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(backward-delete-char-untabify-method (quote hungry))
 '(c-basic-offset 4)
 '(c-cleanup-list (quote (scope-operator compact-empty-funcall)))
 '(c-comment-prefix-regexp
   (quote
	((c-mode . "//+\\|\\**")
	 (c++-mode . "//+\\|\\**")
	 (java-mode . "//+\\|\\**")
	 (pike-mode . "//+!?\\|\\**")
	 (awk-mode . "#+")
	 (other . "//+\\|\\**"))))
 '(c-default-style
   (quote
	((c++-mode . "stroustrup")
	 (java-mode . "java")
	 (awk-mode . "awk")
	 (other . "gnu"))))
 '(c-doc-comment-style
   (quote
	((c-mode . gtkdoc)
	 (c++-mode . gtkdoc)
	 (java-mode . javadoc)
	 (pike-mode . autodoc))))
 '(c-hanging-braces-alist (quote set-from-style))
 '(c-hungry-delete-key t t)
 '(c-ignore-auto-fill (quote (string cpp code)))
 '(c-offsets-alist
   (quote
	((inline-open . 0)
	 (case-label . +)
	 (innamespace . 0))))
 '(calendar-date-style (quote iso))
 '(calendar-daylight-time-zone-name "EST")
 '(calendar-latitude [1 22 north])
 '(calendar-longitude [103 45 east])
 '(calendar-standard-time-zone-name "EST")
 '(calendar-time-zone 480)
 '(case-fold-search t)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 10240)
 '(comment-empty-lines (quote (quote eol)))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(current-language-environment "Latin-1")
 '(custom-safe-themes
   (quote
	("7aaee3a00f6eb16836f5b28bdccde9e1079654060d26ce4b8f49b56689c51904" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" default)))
 '(dabbrev-case-fold-search (quote case-fold-search))
 '(dapprev-case-replace nil)
 '(default-input-method "latin-1-prefix")
 '(display-time-24hr-format t)
 '(display-time-mode t nil (time))
 '(display-time-world-list
   (quote
	(("America/Los_Angeles" "Seattle")
	 ("America/New_York" "New York")
	 ("Europe/London" "London")
	 ("Asia/Calcutta" "New Delhi")
	 ("Asia/Tokyo" "Tokyo")
	 ("Asia/Singapore" "Singapore"))))
 '(electric-indent-mode t)
 '(font-use-system-font t)
 '(global-hl-line-mode t)
 '(hl-line-sticky-flag t)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(large-file-warning-threshold nil)
 '(message-log-max 512)
 '(next-line-add-newlines nil)
 '(nrepl-message-colors
   (quote
	("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
	(idea-darkula-theme intellij-theme python-black material-theme ein tramp ac-cider ac-geiser ac-ispell ac-slime adjust-parens ag alarm-clock auto-complete cider cider-eval-sexp-fu clj-refactor clojure-mode clojure-mode-extra-font-locking dash-functional elpy flycheck flycheck-clojure flycheck-joker flylisp function-args geiser gnugo hackernews html-to-markdown htmlize ido-at-point ido-yes-or-no idomenu imenu-anywhere imenu-list inf-clojure ipython-shell-send javadoc-lookup live-py-mode magit magit-find-file markdown-mode markdown-preview-mode markdown-toc memoize pabbrev paren-face popup-imenu popwin python-mode rubik s scratch shell-switcher shell-toggle smartparens sotclojure sotlisp zenburn-theme flymake dash csv-mode company auto-correct async)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(query-replace-highlight t)
 '(save-abbrevs t)
 '(search-highlight t)
 '(show-paren-mode t nil (paren))
 '(size-indication-mode t)
 '(split-width-threshold nil)
 '(subword-mode t t)
 '(tab-width 4)
 '(truncate-lines nil)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
