(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/ac-dict")
(ac-config-default)