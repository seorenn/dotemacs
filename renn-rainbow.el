(require 'rainbow-mode)

(add-hook 'css-mode-hook (lambda () (rainbow-mode t)))
(add-hook 'js2-mode-hook (lambda () (rainbow-mode t)))
(add-hook 'web-mode-hook (lambda () (rainbow-mode t)))
(add-hook 'lisp-mode-hook (lambda () (rainbow-mode t)))
