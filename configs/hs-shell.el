;;; hs-shell.el --- Shell setup (vterm + eat)

;;; Commentary:
;; Modern shell integration for Emacs.

;;; Code:

;; ------------------------
;; vterm
;; ------------------------
(use-package vterm
  :ensure t
  :commands vterm
  :config
  ;; Better scrollback
  (setq vterm-max-scrollback 10000)

  ;; Kill buffer when process exits
  (setq vterm-kill-buffer-on-exit t)

  ;; Better large output performance
  (setq vterm-buffer-name-string "vterm: %s"))

;; Quick access
(global-set-key (kbd "C-c t v") #'vterm)

;; ------------------------
;; Eat (Emulate A Terminal)
;; ------------------------
(use-package eat
  :ensure t
  :commands (eat eat-other-window)
  :config
  ;; Use widely-supported terminal type
  (setq eat-term-name "xterm-256color")
  (setq xterm-color-preserve-properties t)
  ;; Enable shell integration
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode))

(global-set-key (kbd "C-c t e") #'eat)

;; ------------------------
;; Optional helper functions
;; ------------------------

(defun hs/open-shell ()
  "Open preferred shell."
  (interactive)
  (vterm))

(global-set-key (kbd "C-c t t") #'hs/open-shell)

(provide 'hs-shell)
;;; hs-shell.el ends here
