(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete")
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/ac-dict")
  (setq ac-comphist-file "~/.emacs.d/vendor/auto-complete/ac-comphist.dat")
  (ac-config-default)

  (global-auto-complete-mode t)

  (require 'auto-complete-etags))
