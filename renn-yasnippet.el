(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")

(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/vendor/yasnippet/snippets"))
(yas/global-mode 1)

;;(yas/initialize)
;;(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")

; for nxhtml-mode
(yas/define-snippets 'nxhtml-mode nil 'html-mode)
