;; -----------------------------------------------------------------------------
;; FUNCTIONS
;; -----------------------------------------------------------------------------
;;
;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;;
(defun indent-buffer()
  "Indent the whole buffer from point-min to point-max using the command indent-region"
  (interactive)
  (indent-region 0 (point-max) nil))

;;
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;
(defun my-list-buffers ()
  (interactive)
  (list-buffers)
  (other-window 1))

;; sets the file coding system to unix
(defun set-unix-file-coding-system ()
  (interactive)
  (set-buffer-file-coding-system 'unix))

;;
(defun file-cache-save-cache-to-file (file)
  "Save contents of `file-cache-alist' to FILE.
For later retrieval using `file-cache-read-cache-from-file'"
  (interactive "FFile: ")
  (with-temp-file (expand-file-name file)
    (prin1 file-cache-alist (current-buffer))))

;;
(defun file-cache-read-cache-from-file (file)
  "Clear `file-cache-alist' and read cache from FILE.
  The file cache can be saved to a file using
  `file-cache-save-cache-to-file'."
  (interactive "fFile: ")
  (file-cache-clear-cache)
  (save-excursion
    (set-buffer (find-file-noselect file))
    (beginning-of-buffer)
    (setq file-cache-alist (read (current-buffer)))))

;; -----------------------------------------------------------------------------
;; ADD HOOKS
;; -----------------------------------------------------------------------------
;;
;; deletes all trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "M-<down>") 'forward-paragraph)
(global-set-key (kbd "<backtab>") 'toggle-source-header)

(global-set-key (kbd "C-x C-b") 'my-list-buffers)
(global-set-key (kbd "C-x C-x") 'shell)

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(define-key minibuffer-local-map (kbd "M-RET") 'file-cache-minibuffer-complete)

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))
;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; -----------------------------------------------------------------------------
;; COMMAND ALIASES
;; -----------------------------------------------------------------------------
;;
(defalias 'rb 'rename-buffer)
(defalias 'sh 'shell)

;; -----------------------------------------------------------------------------
;; SET/RESET ATTRIBUTES
;; -----------------------------------------------------------------------------
;;
;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlight current line
(global-hl-line-mode 1)

(setq auto-save-default nil)
(setq electric-indent-mode nil)
(setq menu-bar-mode nil)

(follow-mode t)

(transient-mark-mode t)
(setq-default abbrev-mode t)

(global-font-lock-mode t)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq display-time-day-and-date t)
(display-time)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode t)
(ido-everywhere t)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(projectile-global-mode)
