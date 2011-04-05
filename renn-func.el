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
