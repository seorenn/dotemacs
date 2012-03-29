(add-to-list 'load-path
             "~/.emacs.d/vendor/yasnippet")

(require 'yasnippet)

;;(setq yas/trigger-key (kbd "C-c <kp-multiply>"))

(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")

; for nxhtml-mode
(yas/define-snippets 'nxhtml-mode nil 'html-mode)
