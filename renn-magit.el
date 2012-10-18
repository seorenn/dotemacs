(add-to-list 'load-path "~/.emacs.d/vendor/magit")
(require 'magit)
;; uncomment if needed these:
;; (require 'magit-svn)
;; (require 'magit-topgit)

;; disable vc-git
;; ... not works! (T _ T)
;;(eval-after-load "vc" '(remove-hook 'find-file-hook 'vc-find-file-hook))
