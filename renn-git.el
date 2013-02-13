;; Magit
(add-to-list 'load-path "~/.emacs.d/vendor/magit")
(require 'magit)

;; emacs-git-gutter
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-git-gutter")
(require 'git-gutter)
(global-git-gutter-mode t)
