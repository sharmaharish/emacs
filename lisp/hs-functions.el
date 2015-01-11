;; -----------------------------------------------------------------------------
;; FUNCTIONS
;; -----------------------------------------------------------------------------

;; 90aebf38-b33a-314b-1198-c9bffea2f2a2
(defun uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s_%s_3%s_%s_%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

;;
(defun uuid-insert ()
  "Inserts a new UUID at the point."
  (interactive)
  (insert (upcase (uuid-create))))

;;
(defun indent-buffer()
  "Indent the whole buffer from point-min to point-max using the command indent-region"
  (interactive)
  (indent-region 0 (point-max) nil))

;;
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Switch fromm *.<impl> to *.<head> and vice versa
(defun toggle-source-header ()
  (interactive)
  (when (string-match "^\\(.*\\)\\.\\([^.]*\\)$" buffer-file-name)
    (let ((name (match-string 1 buffer-file-name))
          (suffix (match-string 2 buffer-file-name)))
      (cond ((string-match suffix "cpp\\|cc\\|c\\|CPP\\|CC\\|C\\|hcpp")
             (cond ((file-exists-p (concat name ".hpp"))
                    (find-file (concat name ".hpp")))
                   ((file-exists-p (concat name ".hh"))
                    (find-file (concat name ".hh")))
                   ((file-exists-p (concat name ".h"))
                    (find-file (concat name ".h")))
                   ((file-exists-p (concat name ".HPP"))
                    (find-file (concat name ".HPP")))
                   ((file-exists-p (concat name ".HH"))
                    (find-file (concat name ".HH")))
                   ((file-exists-p (concat name ".H"))
                    (find-file (concat name ".H")))))
            ((string-match suffix "hpp\\|hh\\|h\\|HPP\\|HH\\|H")
             (cond ((file-exists-p (concat name ".cpp"))
                    (find-file (concat name ".cpp")))
                   ((file-exists-p (concat name ".hcpp"))
                    (find-file (concat name ".hcpp")))
                   ((file-exists-p (concat name ".cc"))
                    (find-file (concat name ".cc")))
                   ((file-exists-p (concat name ".c"))
                    (find-file (concat name ".c")))
                   ((file-exists-p (concat name ".CPP"))
                    (find-file (concat name ".CPP")))
                   ((file-exists-p (concat name ".CC"))
                    (find-file (concat name ".CC")))
                   ((file-exists-p (concat name ".C"))
                    (find-file (concat name ".C")))))))))

;; inserts a header guard
(defun make-header-guard (buffer-name)
  (let* ((split-name (split-string  buffer-name "[-_.]"))
         (guard-def
          (concat "_" (mapconcat (lambda (x) (upcase x))
                                 split-name  "_")
                  "_"
                  (upcase (uuid-create)) "_HPP_")))
    guard-def))

;; inserts a header guard
(defun insert-header-guard ()
  (interactive)
  (let ((header-name
         (file-name-nondirectory
          (file-name-sans-extension
           (buffer-file-name (current-buffer))))))
    (if (string= (downcase
                  (file-name-extension
                   (buffer-file-name
                    (current-buffer)))) "hpp")
        (make-header-guard header-name))))

;;
(defun beautify ()
  (interactive)
  (let ((cmd "astyle --style=allman -s4 -Y -f -p -H -y -O -o -k1 -W1 -U --mode=c -z2"))
    (shell-command-on-region (point-min) (point-max) cmd (current-buffer) t)))

;;
(defun my-list-buffers ()
  (interactive)
  (list-buffers)
  (other-window 1))

;; sets the file coding system to unix
(defun set-unix-file-coding-system ()
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun track-shell-directory/procfs ()
  (shell-dirtrack-mode 0)
  (add-hook 'comint-preoutput-filter-functions
            (lambda (str)
              (prog1 str
                (when (string-match comint-prompt-regexp str)
                  (cd (file-symlink-p
                       (format "/proc/%s/cwd" (process-id
                                               (get-buffer-process
                                                (current-buffer)))))))))
            nil t))

;;
(defun file-cache-save-cache-to-file (file)
  "Save contents of `file-cache-alist' to FILE.
For later retrieval using `file-cache-read-cache-from-file'"
  (interactive "FFile: ")
  (with-temp-file (expand-file-name file)
    (prin1 file-cache-alist (current-buffer))))

;;
(defun file-cache-read-cache-from-file (file)
  "Clear `file-cache-alist' and read cache from FILE.
  The file cache can be saved to a file using
  `file-cache-save-cache-to-file'."
  (interactive "fFile: ")
  (file-cache-clear-cache)
  (save-excursion
    (set-buffer (find-file-noselect file))
    (beginning-of-buffer)
    (setq file-cache-alist (read (current-buffer)))))
