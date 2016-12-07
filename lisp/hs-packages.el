(require 'cl)
(require 'filecache)

(defconst enable-autocomplete t)
(defconst enable-cedet nil)
(defconst enable-clojure t)
(defconst enable-ensime nil)
(defconst enable-go nil)
(defconst enable-gtags nil)
(defconst enable-haskell nil)
(defconst enable-magit t)
(defconst enable-org t)
(defconst enable-scala nil)
(defconst enable-slime nil)
(defconst enable-sqlplus nil)
(defconst enable-w3m nil)
(defconst enable-yasnippet t)
(defconst enable-ido t)
(defconst enable-erlang nil)
(defconst enable-paredit t)
(defconst enable-geiser nil)
(defconst enable-ag t)
(defconst enable-company t)

(defconst package-root (prefix-home "packages/"))

(add-to-list 'load-path package-root)

(when enable-company
  (add-to-list 'load-path (concat package-root "company-mode"))
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode))

(when enable-ag
  (add-to-list 'load-path (concat package-root "dash"))
  (add-to-list 'load-path (concat package-root "s"))
  (add-to-list 'load-path (concat package-root "ag"))
  (require 'ag))

(when enable-erlang
  (setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.7/emacs" load-path))
  (setq erlang-root-dir "/usr/local/lib/erlang")
  (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
  (require 'erlang-start))

;; sqlplus package settings
(when enable-sqlplus
  (require 'sqlplus)
  (add-hook 'sqlplus-mode-hook
            '(lambda ()
               (local-set-key (kbd "M-c") 'sqlplus-send-current))))

;; yasnippet package settings
(when enable-yasnippet
  (add-to-list 'load-path (concat package-root "yasnippet"))
  (require 'yasnippet)
  (yas-global-mode 1))

;; global / gtags package settings
(when enable-gtags
  (require 'gtags)
  ;; key bindings
  (global-set-key (kbd "<f9>") 'gtags-find-tag)
  (global-set-key (kbd "<f10>") 'gtags-find-rtag)
  (global-set-key (kbd "<f11>") 'gtags-find-file)
  (global-set-key (kbd "<f12>") 'gtags-find-tag-from-here))

;; cedet package settings
(when enable-cedet
  (load-file (concat package-root "cedet/common/cedet.el"))
  (require 'semantic-ia)
  (require 'semantic-gcc)
  ;;(require 'semantic-global)
  (global-ede-mode 1)                  ; Enable the Project management system
  (semantic-load-enable-code-helpers))  ; Enable prototype help and smart completion

;; slime package settings
(when enable-slime
  (setq inferior-lisp-program "/usr/local/Cellar/sbcl/1.2.2/bin/sbcl")
                                        ;; (setq inferior-lisp-program "/usr/bin/clisp")
  ;; (setq inferior-lisp-program "/home/harish/bin/ecl-11.1.1/bin/ecl")
  (add-to-list 'load-path (concat package-root "slime"))
  (require 'slime)
  ;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  ;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode)))
  (slime-setup '(slime-fancy))
  ;; (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
  )

;; w3m package settings
(when enable-w3m
  (require 'w3m-load)
  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (setq w3m-use-cookies t))

;; org package settings
(when enable-org
  (require 'org-install)
  ;; org mode config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  (setq org-agenda-files (prefix-home "/todo.org")))

;; haskell mode
(when enable-haskell
  (load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(when enable-scala
  (add-to-list 'load-path (concat package-root "scala"))
  (require 'scala-mode-auto))

(when enable-clojure
  (add-to-list 'load-path (concat package-root "clojure-mode"))
  (require 'clojure-mode))

(when enable-ensime
  (add-to-list 'load-path (concat package-root "ensime/elisp"))
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  (eval-after-load "scala-mode"
    '(progn
       (define-key scala-mode-map (kbd "<f9>") 'ensime-builder-build)
       (define-key scala-mode-map (kbd "<f10>") 'ensime-inf-switch)))
  (eval-after-load "scala-mode"
    '(progn
       (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
       (define-key scala-mode-map (kbd "<f9>") 'scala-run)
       (define-key scala-mode-map (kbd "RET") 'newline-and-indent)
       ))
  (defun scala-run ()
    (interactive)
    (ensime-sbt-action "run")
    (ensime-sbt-action "~compile")
    (let ((c (current-buffer)))
      (switch-to-buffer-other-window
       (get-buffer-create (ensime-sbt-build-buffer-name)))
      (switch-to-buffer-other-window c))))
  ;; (setq exec-path
  ;;       (append exec-path (list "~/.opt/scala/bin"))))

(when enable-autocomplete
  (add-to-list 'load-path (concat package-root "auto-complete"))
  (add-to-list 'load-path (concat package-root "popup"))
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (prefix-home "auto-complete/dict"))
  (ac-config-default))

(when enable-go
  (require 'go-mode))

(when enable-magit
  (add-to-list 'load-path (concat package-root "magit/lisp"))
  (add-to-list 'load-path (concat package-root "with-editor"))
  (require 'magit)
  (defalias 'ms 'magit-status))

(when enable-ido
  (require 'ido)
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t))

(when enable-paredit
  (add-to-list 'load-path (concat package-root "paredit"))
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code" t)
  (require 'paredit)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))

(when enable-geiser
  (add-to-list 'load-path (concat package-root "geiser/elisp"))
  (require 'geiser))
