(defvar rennfiles-dirs '("~/Dropbox/notes"))

(defvar rennfiles-mode-hook nil)

(defvar rennfiles-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for the rennfiles major mode")

(defun rennfiles-get-files (dir)
  (when (file-exists-p dir)
    (directory-files-and-attributes dir)
    )
  )

(defun rennfiles-get-files-info (dir)
  (let ((result nil)
        (tmp nil)
        (cfn nil)
        (cftype nil))
    (setq tmp (rennfiles-get-files dir))
    (when tmp
      (dolist (e tmp)
        (setq cfn (car e))
        (setq cftype (nth 1 e))
        (when (and (not (string= cfn ".")) (not (string= cfn "..")))
          (when cftype
            (setq cfn (concat cfn "/"))
            )
          (setq result (append result (list cfn)))
          )
        )
      )
    result)
  )

(defun rennfiles-make-buffer ()
  (let ((flist nil))
    (setq flist (rennfiles-get-files-info "~/"))
    (dolist (e flist)
      (append-to-buffer)
      (insert e)
      (insert "\n")
      )
    )
  )

(define-derived-mode rennfiles-mode fundamental-mode "rennfiles-mode"
  (use-local-map rennfiles-mode-map)
  )

(defun rennfiles-mode-setup ()
  (switch-to-buffer (get-buffer-create "*RennFiles*"))
  (kill-all-local-variables)
  (use-local-map rennfiles-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  
  (setq major-mode 'rennfiles-mode
        mode-name "RennFiles"
        mode-line-process ""
        truncate-lines t)
  (run-hooks 'rennfiles-mode-hook)
  )

(defun rennfiles ()
  (interactive)
  (rennfiles-mode-setup)
  (rennfiles-make-buffer)
  )

(provide 'rennfiles)