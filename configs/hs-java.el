(add-hook 'java-mode-hook
          (lambda ()
            (setq compilation-read-command nil)
            (local-set-key (kbd "<f5>") 'compile)
            (set (make-local-variable 'compile-command)
                 (concat (prefix-home "scripts/runp ") buffer-file-name))))
