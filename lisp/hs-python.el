
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'compile)
            (set (make-local-variable 'compile-command)
                 (concat (prefix-home "scripts/runp ") buffer-file-name))))
