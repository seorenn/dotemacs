(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/vendor/yasnippet/snippets" "~/.emacs.d/snippets"))
(yas-global-mode 1)

;;;; for nxhtml-mode
;(yas-define-snippets 'nxhtml-mode nil 'html-mode)
