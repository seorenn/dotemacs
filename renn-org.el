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

(setq org-return-follows-link t)
(setq org-tab-follows-link t)

(eval-after-load "org"
  '(setq  org-emphasis-regexp-components
		  '(" \t('\"{" 
			"- \t.,:!?;'\")}[:multibyte:]" 
			" \t\r\n,\"'" 
			"." 
			1)))

(add-hook 'org-mode-hook
		  (let ((original-command (lookup-key org-mode-map [tab])))
			`(lambda ()
			   (setq yas/fallback-behavior
					 '(apply ,original-command))
			   (auto-fill-mode)
			   (local-set-key [tab] 'yas/expand))))

;; for MacTexs
(setq org-export-latex-default-class "article")
