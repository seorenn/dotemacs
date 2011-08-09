;; Configurations for JavaScript Editing...

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-consistent-level-indent-inner-bracket-p t)
(setq js2-auto-indent-p nil)
(setq js2-idle-timer-delay 2)
(setq js2-dynamic-idle-timer-adjust 2)
(setq js2-use-ast-for-indentation-p t)
(setq js2-enter-indents-newline t)
(setq js2-basic-offset 2)
(setq js2-mirror-mode t)
(setq js2-bounce-indent-p t)

;; jade-mode
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; turn-off autopair mode when js2-mode activated
(add-hook 'js2-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))

(provide 'renn-js2mode)