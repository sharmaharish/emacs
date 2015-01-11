;;   __    ___         ___ _ __ ___   __ _  ___ ___
;;  |  |__/ __|  __   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  _  \__ \ |__| |  __/ | | | | | (_| | (__\__ \
;;  |__|__|__ /       \___|_| |_| |_|\__,_|\___|___/
;;

(defconst emacs-home (concat (getenv "HOME") "/.emacs.d"))
(defun prefix-home (path) (concat emacs-home "/" path))

(add-to-list 'load-path (prefix-home "lisp"))

(load "hs-packages.el")
(load "hs-functions.el")
(load "hs-default.el")

(shell)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(backup-directory-alist (quote (("." . "/Users/harish/.emacs.d/bakup"))))
 '(backward-delete-char-untabify-method (quote hungry))
 '(c++-font-lock-extra-types
   (quote
    ("\\sw+_t" "FILE" "lconv" "tm" "va_list" "jmp_buf" "istream" "istreambuf" "ostream" "ostreambuf" "ifstream" "ofstream" "fstream" "strstream" "strstreambuf" "istrstream" "ostrstream" "ios" "string" "rope" "list" "slist" "deque" "vector" "bit_vector" "set" "multiset" "map" "multimap" "hash" "hash_set" "hash_multiset" "hash_map" "hash_multimap" "stack" "queue" "priority_queue" "type_info" "iterator" "const_iterator" "reverse_iterator" "const_reverse_iterator" "reference" "const_reference" "true" "false" "noncopyable" "shared_ptr")))
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
 '(calendar-daylight-time-zone-name "SGT")
 '(calendar-latitude [1 22 north])
 '(calendar-longitude [103 45 east])
 '(calendar-standard-time-zone-name "SGT")
 '(calendar-time-zone 480)
 '(case-fold-search t)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 10240)
 '(comment-empty-lines (quote (quote eol)))
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(current-language-environment "Latin-1")
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
     ("Asia/Dubai" "Dubai")
     ("Asia/Calcutta" "Bangalore")
     ("Asia/Tokyo" "Tokyo")
     ("Asia/Singapore" "Singapore"))))
 '(electric-indent-mode t)
 '(font-use-system-font t)
 '(global-hl-line-mode t)
 '(hl-line-sticky-flag t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-whole-line t)
 '(large-file-warning-threshold nil)
 '(message-log-max 512)
 '(next-line-add-newlines nil)
 '(query-replace-highlight t)
 '(require-final-newline t)
 '(save-abbrevs t)
 '(scheme-program-name "guile")
 '(search-highlight t)
 '(show-paren-mode t nil (paren))
 '(size-indication-mode t)
 '(split-width-threshold nil)
 '(subword-mode t t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-lines nil)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "grey10" :foreground "grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(comint-highlight-prompt ((((min-colors 88) (background dark)) (:inherit font-lock-type-face))))
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "yellow" :foreground "white"))))
 '(ediff-even-diff-Ancestor ((((class color) (min-colors 16)) (:background "grey40" :foreground "white"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "grey40" :foreground "white"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "grey40" :foreground "white"))))
 '(font-lock-builtin-face ((t (:foreground "LightSeaGreen"))))
 '(font-lock-comment-face ((t (:foreground "IndianRed"))))
 '(font-lock-constant-face ((t (:foreground "brightwhite"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t (:foreground "DarkSeaGreen"))))
 '(font-lock-keyword-face ((t (:foreground "aquamarine"))))
 '(font-lock-string-face ((t (:foreground "SandyBrown"))))
 '(font-lock-type-face ((t (:foreground "LightSeaGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightBlue"))))
 '(font-lock-warning-face ((t (:foreground "tomato"))))
 '(highlight ((t (:background "grey15"))))
 '(show-paren-match ((nil (:background "green" :foreground "black")))))
