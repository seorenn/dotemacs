(require 'projectile)
(require 'helm-projectile)

(projectile-global-mode)

;;;; Integrated find-file
;; If current working directory is project, use help-projectile
;; Else, use find-file
(defun sr-open-file ()
  "Open file using projectile+Helm or ido"
  (interactive)
  (if (projectile-project-p)
      (helm-projectile)
    (ido-find-file)))
