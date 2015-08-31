;; Magit

(require 'magit)

; Do not shows The Magit 1.4.0 Warning Message
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil)

;; git-gutter

(require 'git-gutter)

(setq git-gutter:window-width 2)

(global-git-gutter-mode t)
