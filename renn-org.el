(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.todo$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

(setq org-directory "~/Dropbox/notes")
(setq org-agenda-files (list "~/Dropbox/notes/todo.org"
                             "~/Dropbox/notes/worktodo.org"))

(setq org-startup-truncated t)