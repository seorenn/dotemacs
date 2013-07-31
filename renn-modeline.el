(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(setq-default mode-line-format
      (list
       " "
       '(:eval (propertize "%b" 'face 'font-lock-type-face
                           'help-echo (buffer-file-name)))

       " ("
       (propertize "%02i" 'face 'font-lock-type-face) ","
       (propertize "%02c" 'face 'font-lock-type-face)
       ")"

       "["
       (propertize "%p" 'face 'font-lock-constant-face)
       "/"
       (propertize "%I" 'face 'font-lock-constant-face)
       "]"

       "["
       '(:eval (propertize "%m" 'face 'font-lock-comment-face
                           'help-echo buffer-file-coding-system))
       "]"

       "["
       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                           'face 'font-lock-reference-face
                           'help-echo (concat "Buffer is in "
                                              (if overwrite-mode "overwrite" "insert") " mode")))
       '(:eval (when (buffer-modified-p)
                 (concat "," (propertize "Mod"
                                         'face 'font-lock-regexp-grouping-construct
                                         'help-echo "Buffer has been modified"))))
       '(:eval (when buffer-read-only
                 (concat "," (propertize "RO"
                                         'face 'font-lock-type-face
                                         'help-echo "Buffer is read-only"))))
       "] "
       "%-"))
