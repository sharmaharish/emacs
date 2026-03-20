(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; (add-hook 'rust-mode-hook
;;           (lambda () (prettify-symbols-mode)))

(setq rust-format-on-save t)
