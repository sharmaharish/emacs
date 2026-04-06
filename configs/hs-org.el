(require 'org)
(require 'org-capture)

(setq org-directory "~/Projects/org")

(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'org-indent-mode)

(defun hs/journal-file ()
  (expand-file-name
   (format "journal-%s.org" (format-time-string "%Y"))
   org-directory))

(defun hs/finance-file ()
  (expand-file-name
   (format "finance-%s.org" (format-time-string "%Y"))
   org-directory))

(defun hs/open-journal ()
  "Open current year's journal file."
  (interactive)
  (find-file (hs/journal-file)))

(defun hs/open-finance ()
  "Open current year's finance file."
  (interactive)
  (find-file (hs/finance-file)))

(defun hs/journal-file ()
  (concat org-directory "/journal-" (format-time-string "%Y") ".org"))

(defun hs/finance-file ()
  (concat org-directory "/finance-" (format-time-string "%Y") ".org"))

(setq org-capture-templates
      '(("j" "Journal"
         entry
         (file+olp+datetree (function hs/journal-file))
         "* %?\nEntered on %U\n")

        ("t" "Transaction"
         entry
         (file+olp+datetree (function hs/finance-file))
         "- %^{Description} :%^{Type|expense|income}:%^{Category}: $%^{Amount}\nEntered on %U\n")))

(global-set-key (kbd "C-c c") #'org-capture)

(global-set-key (kbd "C-c o j") #'hs/open-journal)
(global-set-key (kbd "C-c o f") #'hs/open-finance)
