;; org-mode
(defvar my-org-packages
  '(
    dash
    f
    s
    ; emacsql
    magit-section

    org
    org-roam
    org-journal

    visual-fill-column
    writeroom-mode
    writegood-mode
    ))

(dolist (p my-org-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; org config
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-cycle-hide-block-startup t)
(setq org-log-done-with-time t)
(setq org-log-done t)

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "*")))))) // replace * with proper dot

(defun hs/org-faces ()
  (set-face-attribute 'org-level-1 nil :height 1.1)
  (set-face-attribute 'org-level-2 nil :height 1.0)
  (set-face-attribute 'org-level-3 nil :height 1.0)
  (set-face-attribute 'org-level-4 nil :height 1.0)
  (set-face-attribute 'org-level-5 nil :height 1.0)
  (set-face-attribute 'org-document-title nil :height 1.2))

(setq org-agenda-files '("~/Projects/HarishDocuments/org"))
(setq org-journal-dir "~/Projects/HarishDocuments/journal")
(setq org-roam-directory (file-truename "~/Projects/HarishDocuments/org-roam"))

(org-roam-db-autosync-mode)

(add-hook 'org-mode-hook 'visual-line-mode)
; (add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'company-mode)
(add-hook 'org-mode-hook 'hs/org-faces)

(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
