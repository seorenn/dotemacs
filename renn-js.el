;; Configurations for JavaScript Editing...

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-consistent-level-indent-inner-bracket-p t)
(setq js2-auto-indent-p t)
(setq js2-idle-timer-delay 0.5)
(setq js2-use-ast-for-indentation-p t)
(setq js2-enter-indents-newline t)
(setq js2-basic-offset 2)

(provide 'renn-js2mode)