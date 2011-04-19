;; set these to renn-private.el
;; (setq org-directory "...")

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.todo$" . org-mode))

(setq org-agenda-files (list ("~/Dropbox/notes/todo.org"
                              "~/Dropbox/notes/worktodo.org")))