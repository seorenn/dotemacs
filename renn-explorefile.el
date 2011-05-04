;;; renn-explorefile.el - Find file in current or parent directories

(defun get-parent-path (dir)
  ;; This function has bug that has determine slash-char after backslash-char
  (let (dirs)
    (setq dirs (split-string dir "/"))
    (setq dirs (reverse dirs))
    (setq dirs (cdr dirs))
    (setq dirs (reverse dirs))
    (mapconcat 'identity dirs "/")))

(defun remove-last-slash (path)
  (when (= (aref path (1- (length path))) ?/)
    (setq path (substring path 0 (1- (length path)))))
  path)

(defun explore-file-recrv (tdir fn home-only)
  (let (home-dir
        result
        target-path
        (curdir (remove-last-slash tdir)))
    (if home-only
        (setq home-dir (expand-file-name "~"))
      (setq home-dir "/"))
    (while (and (> (length curdir) 0) (string-match home-dir curdir) (not result))
      (progn
        (setq target-path (concat curdir (concat "/" fn)))
        (when (file-exists-p target-path)
          (setq result target-path))
        ;; (message (concat "-> " target-path))
        (setq curdir (get-parent-path curdir))))
  result))
  
(defun explore-file (fn home-only)
  "Find file or directory in current or parent directories."
  (interactive)
  (let (cur-dir result)
    (when (buffer-file-name)
      (progn
        (setq cur-dir (file-name-directory (buffer-file-name)))
        (setq result (explore-file-recrv cur-dir fn home-only))))
    result))

(provide 'explore-file)

(defvar appended-tag-path (list) "TAG Path List")

(defun str-in-list? (str l)
  (interactive)
  (let (result cur)
    (setq cur (car l))
    (while (and cur (not result))
      (progn
        (when (string= cur str)
          (setq result t))
        (setq l (cdr l))
        (setq cur (car l))))
    result))

(defun find-tag-and-add-table ()
  "Find TAG file and return if not found file path"
  (interactive)
  (let (fpath result)
    (setq fpath (explore-file "TAGS" t))
    (unless (str-in-list? fpath appended-tag-path)
      (add-to-list 'appended-tag-path fpath)
      (setq result fpath))
    result))

(defun explore-tag ()
  "Find TAG and set to Tag Table"
  (interactive)
  (let (fpath)
    (setq fpath (find-tag-and-add-table))
    (when fpath
      (visit-tags-table fpath nil))))

;;(add-hook 'python-mode-hook 'explore-tag)
