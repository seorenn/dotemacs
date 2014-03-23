(require 'projectile)
(require 'helm-projectile)

(projectile-global-mode)
(setq projectile-indexing-method 'native) ; use system command like find, git...
(setq projectile-enable-caching nil)
(setq projectile-require-project-root t)

;;;; Integrated find-file
;; If current working directory is project, use help-projectile
;; Else, use find-file
(defun sr-open-file ()
  "Open file using projectile+Helm or ido"
  (interactive)
  (if (projectile-project-p)
      (helm-projectile)
    (helm-for-files)))
