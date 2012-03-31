;; Configurations for JavaScript Editing...

;; Configurations for js3-mode
(add-to-list 'load-path "~/.emacs.d/vendor/js3-mode")
(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

(setq js3-lazy-commas t)
(setq js3-lazy-operators t)
(setq js3-lazy-dots t)
(setq js3-expr-indent-offset 2)
(setq js3-paren-indent-offset 2)
(setq js3-square-indent-offset 2)
(setq js3-curly-indent-offset 2)
;(setq js3-indent-level 2)
(setq js3-auto-indent-p t)
(setq js3-enter-indents-newline t)
(setq js3-indent-on-enter-key t)

;; jade-mode
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(provide 'renn-js)