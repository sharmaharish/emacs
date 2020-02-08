;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-python-packages
  '(elpy             ;; emacs lisp python environment
    flycheck         ;; on the fly syntax checking
    blacken   ;; black formattign on save for python code
    py-autopep8      ;; run autopep8 on save
    ;; ein              ;; emacs IPython notebook
    ))

(dolist (p my-python-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'py-autopep8)

(setq python-shell-interpreter "python3")

(elpy-enable)

;; Use IPython for REPL
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;;
;; (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'compile)
            (set (make-local-variable 'compile-command)
                 (concat (prefix-home "scripts/runp ") buffer-file-name))))
