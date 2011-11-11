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

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun smart-beginning-of-line2 ()
  "Move cursor to beginning of line"
  (interactive)
  (if (and (eq last-command 'smart-beginning-of-line)
           (/= (line-beginning-position) (point)))
      (beginning-of-line)
    (beginning-of-line-text))
  )

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

;; goto matched parenthesis
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on (){}[], similar to vi style of %."
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

;; open url in current line
(defun renn-open-url ()
  (interactive)
  (let (line url)
    (save-excursion
      ;(setq line (thing-at-point 'line))
      (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      ;;(string-match "\\([a-z]+\\:\\/\\/[^\s-]+\\)" line)
      (string-match "\\([a-z]+\\:\\/\\/[^\r\n\t ]+\\)" line)
      (browse-url (match-string 1 line)))))

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
        (setq tmp-attr (file-attributes (car tmp-attr) t)))
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

(defun files-from-attr-list-with-except (file-attrs excepts)
  "File list of inputed file-attrs except containing excepts pattern"
  (interactive)
  (let (result)
    (mapcar
     (lambda (each-attr)
       (when (string-not-match-list (car each-attr) excepts)
         (when (is-file-attr (cdr each-attr))
           (append result (list (car each-attr))))))
     file-attrs)
    result))

(defun files-each-dir-with-except (target-dirs excepts)
  "Files list of target-dirs's each directory except containing user defined pattern"
  (interactive)
  (let (result tmp-files tmp-file-name)
    (mapcar
     (lambda (dir)
       (setq dir (expand-file-name dir))
       (message (concat "Contacting directory " dir))
       (setq result (append result (list dir)))
       (setq tmp-files (directory-files-and-attributes dir t))
       (mapcar
        (lambda (each-attr)
          (when (string-not-match-list (car each-attr) excepts)
            ;;(when (is-file-attr (cdr each-attr))
            (when (not (file-accessible-directory-p (car each-attr)))
              (setq result (append result (list (car each-attr)))))))
        tmp-files))
     target-dirs)
    result))
       
(setq my-index-paths '("~/.emacs.d"
                       "~/Devel/apiserver"
                       "~/Dropbox/notes"))
(setq my-index-excepts
      '("\\(\\.pyc\\|\\.elc\\|\\.out\\)$"
        "\\(\\.jpg\\|\\.gif\\|\\.png\\|\\.jpeg\\|\\.bmp\\)$"
        "\\(\\.mpg\\|\\.mpeg\\|\\.mp4\\|\\.avi\\|\\.wmv\\|\\.mkv\\)$"
        "\\(\\.mp3\\|\\.wav\\|\\.aac\\)$"))

(setq my-index-output-file "~/.myfiles-index")

(defun index-my-files ()
  (interactive)
  (let (tmp-list)
    (with-temp-buffer
      (message (concat "Writing " my-index-output-file))
      (setq tmp-list
            (files-each-dir-with-except my-index-paths my-index-excepts))
      (mapcar
       (lambda (x)
         (insert x)
         (insert "\n"))
       tmp-list)
      (write-region (point-min)
                    (point-max)
                    my-index-output-file))))
      
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

;; Convert kanji to hiragana
(defun japanese-to-kanji (string)
  "Return t if string contains Japanese kanji"
  (let ((length (length string))
        (count 0)
        (ret nil)
        (char ?a))
    (while (< count length)
      (setq char (aref string count))
      (if (and (< char ?\u9fa5)
               (> char ?\u4e00))
          (setq ret t))
      (setq count (1+ count)))
    ret))

;;
;; Copy/Cut for single line
;;

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

;; -----