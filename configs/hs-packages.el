;; (require 'cl)
;; (require 'filecache)

(defconst enable-gtags nil)
(defconst enable-org nil)
(defconst enable-slime nil)

;; (defconst package-root (prefix-home "packages/"))

;; (add-to-list 'load-path package-root)

;; global / gtags package settings
(when enable-gtags
  (require 'gtags)
  ;; key bindings
  (global-set-key (kbd "<f9>") 'gtags-find-tag)
  (global-set-key (kbd "<f10>") 'gtags-find-rtag)
  (global-set-key (kbd "<f11>") 'gtags-find-file)
  (global-set-key (kbd "<f12>") 'gtags-find-tag-from-here))

;; slime package settings
(when enable-slime
  (setq inferior-lisp-program "/usr/local/Cellar/sbcl/1.2.2/bin/sbcl")
                                        ;; (setq inferior-lisp-program "/usr/bin/clisp")
  ;; (setq inferior-lisp-program "/home/harish/bin/ecl-11.1.1/bin/ecl")
  ;; (add-to-list 'load-path (concat package-root "slime"))
  (require 'slime)
  ;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  ;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode)))
  (slime-setup '(slime-fancy))
  ;; (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
  )

;; org package settings
(when enable-org
  (require 'org-install)
  ;; org mode config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  (setq org-agenda-files (prefix-home "/todo.org")))
