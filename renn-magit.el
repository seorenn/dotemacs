(add-to-list 'load-path "~/.emacs.d/vendor/magit")
(require 'magit)
;; uncomment if needed these:
;; (require 'magit-svn)
;; (require 'magit-topgit)

;; disable vc-git
(eval-after-load "vc" '(remove-hook 'find-file-hook 'vc-find-file-hook))