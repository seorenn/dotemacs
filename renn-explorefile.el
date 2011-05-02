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
