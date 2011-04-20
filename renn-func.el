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

;; reverse other-window
(defun other-window-reverse ()
  (interactive)
  (other-window -1))

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

(defun is-file-attr (file-attr)
  (let (result (tmp-attr file-attr))
    (setq result nil)
    (when (stringp (car tmp-attr))
      (setq tmp-attr (file-attributes (car tmp-attr))))
    (unless (car tmp-attr) (setq result t))
    result))

(defun files-under-dir-with-except (directory except-list)
  "List the files in DIRECTORY and in its sub-directories except some directories and extensions"
  (interactive)
  (let (tmp-files-list
        cur-path
        cur-path-type
        (current-directory-list
         (directory-files-and-attributes directory t)))
    (message (concat "navigate directory " directory))
    ;; while we are in the current directory
    (while current-directory-list
      (progn
        (setq cur-path (car (car current-directory-list)))
        (setq cur-path-type (nth 1 (car current-directory-list)))
        (when (string-not-match-list cur-path except-list)
          ;; if type is file, append to tmp-files-list
          ;;(if (not cur-path-type)
          (if (is-file-attr(cdr (car current-directory-list)))
              ;; true
              (setq tmp-files-list
                    (cons cur-path tmp-files-list))
            ;; false
            ;; recursive-indexing
            (unless
                (equal "."
                       (substring cur-path -1))
              (setq tmp-files-list
                    (append
                     tmp-files-list
                     (files-under-dir-with-except
                      cur-path
                      except-list))))))
        ;; pop from current-directory-list
        (setq current-directory-list (cdr current-directory-list))))
    ;; return
    tmp-files-list))

(defun index-files-and-save (target-file-name target-dir except-list)
  "Index all files of HOME directory except containing user defined symbols."
  (interactive)
  (let ((tmp-list (files-under-dir-with-except
                   target-dir
                   except-list)))
    (with-temp-buffer
      (mapcar (lambda (f)
                (insert f)
                (insert "\n"))
              tmp-list)
      (when (file-writable-p target-file-name)
        (message (concat "writing " target-file-name))
        (write-region (point-min)
                      (point-max)
                      target-file-name)))))

;; (defun index-home-files ()
;;   "Index all files without some directory and file."
;;   (interactive)
;;   (index-files-and-save "~/.indexed_files.txt"
;;                         "~"
;;                         '("\\.Trash.*"
;;                           "\\.macports.*"
;;                           "\\.git.*"
;;                           "\\.DS_Store"
;;                           "\\/Backup"
;;                           "\\/Private"
;;                           "\\/Dropbox\\/devel"
;;                           "\\/Dropbox\\/webdevel"
;;                           "\\/Library"
;;                           "\\/Downloads\\/targets"
;;                           "\\.wmv$"
;;                           "\\.avi$"
;;                           "\\.mpg$"
;;                           "\\.mp4$"
;;                           "\\.mp3$"
;;                           "\\.jpg$"
;;                           "\\.jpeg$"
;;                           "\\.png$"
;;                           "\\.gif$"
;;                           "\\.rar$"
;;                           "\\.zip$"
;;                           "\\.7z$")))