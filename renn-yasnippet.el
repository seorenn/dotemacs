(require 'yasnippet)
;(setq yas/snippet-dirs '(list "~/.emacs.d/snippets" yas/snippet-dirs))
(yas-load-directory "~/.emacs.d/snippets")
(yas-global-mode 1)

;;;; for nxhtml-mode
;(yas-define-snippets 'nxhtml-mode nil 'html-mode)
