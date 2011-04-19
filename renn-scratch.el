;; ------- follows code under development ----------------------

(defun renn-buffer-setup ()
  "test"
  (interactive)
  (let (tmp-files tmp-fn i)
    (message (concat "visiting " buffer-file-name))
    (message (concat "directory " (file-name-directory buffer-file-name)))
    (message (concat "home " (expand-file-name "~")))

    (setq tmp-files (directory-files (file-name-directory buffer-file-name)))
    (setq i 0)
    ;;(message (concat "file count: " (length tmp-files)))

    ;; (when (< i (length tmp-files))
    ;;   (setq tmp-fn (nth i tmp-files))
    ;;   (message (concat "file: " tmp-fn))
    ;;   (setq i (1+ i))
    ;;   )
    (mapcar (lambda (fn)
              (message (concat "file: " fn))
              ) tmp-files)
          
  ))

;;(add-hook 'find-file-hook 'renn-buffer-setup)
