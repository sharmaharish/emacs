;; org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-RET") 'org-insert-todo-heading)))
