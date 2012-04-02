(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/ac-dict")
(setq ac-comphist-file "~/.emacs.d/vendor/auto-complete/ac-comphist.dat")
(ac-config-default)

(require 'auto-complete-etags)

;; for js3-mode
(setq ac-modes (append ac-modes (list 'js2-mode 'js3-mode)))
;(custom-add-frequent-value 'ac-modes 'js3-mode)