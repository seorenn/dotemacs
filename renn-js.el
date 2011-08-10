;; Configurations for JavaScript Editing...

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-enter-indents-newline t)
(setq js2-basic-offset 2)

;; jade-mode
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; turn-off autopair mode when js2-mode activated
;;(add-hook 'js2-mode-hook
;;          #'(lambda () (setq autopair-dont-activate t)))

(provide 'renn-js2mode)