(defun reload-dotemacs ()
  "Reload .emacs"
  (interactive)
  (let (dotemacs-default dotemacs-window)
    (setq dotemacs-default "~/.emacs")
    (setq dotemacs-window "~/_emacs")
    (if (file-exists-p dotemacs-default) (progn
                                     (message "Load %s" dotemacs-default)
                                     (load-file dotemacs-default)
                                     )
      (if (file-exists-p dotemacs-window) (progn 
                                       (message "Load %s" dotemacs-window)
                                       (load-file dotemacs-window)
                                       )
        (message "Could not found dotemacs")
        ))))

(defun build-tags (dir-name)
  "Build TAGS file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s/TAGS -e -R %s" dir-name (directory-file-name dir-name))))

(defun load-my-library (fname)
  "load-library if file(fname) was exists..."
  (if (file-exists-p (concat (concat "~/.emacs.d/" fname) ".el")) (progn
                                             (load-library fname))))

(defun open-google-translate ()
  "Translate current word using Google Translator"
  (interactive)
  (let (sel-word target-url)
    (setq sel-word
          (if (and transient-mark-mode mark-active)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))

    ;(setq sel-word (replace-regexp-in-string " " "%20" sel-word))
    (setq sel-word (url-hexify-string sel-word))
    (setq target-url (concat "http://translate.google.com/#auto|ko|" sel-word))

    (browse-url target-url)))

;; I-search with initial contents.
;; original source: http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun files-in-below-directory (directory)
  "List the .el files in DIRECTORY and in its sub-directories."
  ;; Although the function will be used non-interactively,
  ;; it will be easier to test if we make it interactive.
  ;; The directory will have a name such as
  ;;  "/usr/local/share/emacs/21.0.100/lisp/"
  (interactive "DDirectory name: ")
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in `.el'
       ;; and if so, append its name to a list.
       ((equal ".el" (substring (car (car current-directory-list)) -3))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; else descend into the directory and repeat the process
          (setq el-files-list
                (append
                 (files-in-below-directory
                  (car (car current-directory-list)))
                 el-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    el-files-list))

;; TODO: add to list when file, not directory.
;;       recursive navigation not works well...
(defun string-not-match-list (src-string pattern-list)
  "Evaluate src-string not matches pattern-list's each pattern."
  (let (result (tmp-pattern-list pattern-list))
    (setq result t)
    (while tmp-pattern-list
      (if (string-match (car tmp-pattern-list) src-string)
          (progn (setq result nil)
                 (setq tmp-pattern-list nil)))
      (setq tmp-pattern-list (cdr tmp-pattern-list)))
    result))

; TODO - check exception list
(defun files-under-dir-with-except (directory except-list)
  "List the files in DIRECTORY and in its sub-directories except some directories and extensions"
  (interactive)
  (let (tmp-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; test
    (message (concat "navigate " directory))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in `.el'
       ;; and if so, append its name to a list.
       ;; ((equal ".el" (substring (car (car current-directory-list)) -3))
       ;;  (setq tmp-files-list
       ;;        (cons (car (car current-directory-list)) tmp-files-list)))
       ((string-not-match-list (car (car current-directory-list)) except-list)
           (setq tmp-files-list
                 (cons (car (car current-directory-list)) tmp-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; else descend into the directory and repeat the process
          (setq tmp-files-list
                ;; (append
                ;;  (files-in-below-directory
                ;;   (car (car current-directory-list)))
                ;;  tmp-files-list)))))
                (append
                 (files-under-dir-with-except
                  (car (car current-directory-list))
                  except-list)
                 tmp-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    tmp-files-list))
