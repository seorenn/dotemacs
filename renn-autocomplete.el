;;;; Auto Complete

(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'js2-mode)
(add-to-list 'ac-modes 'js3-mode)
(add-to-list 'ac-modes 'html-mode)

(require 'auto-complete-etags)
