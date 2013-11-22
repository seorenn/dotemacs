(require 'clojure-mode)
(require 'nrepl)
(require 'ac-nrepl)
(require 'align-cljlet)

(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl nil)
(add-to-list 'same-window-buffer-names "*nrepl*")
(setq nrepl-history-file "~/.emacs.d/nrepl-history")
(setq nrepl-popup-stacktraces nil)

(add-hook 'nrepl-connected-hook
          (defun renn-clojure-mode-eldoc-hook ()
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
            (nrepl-enable-on-existing-clojure-buffers)))

(add-hook 'nrepl-mode-hook 'subword-mode)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)

(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)
            (define-key nrepl-mode-map
              (kbd "{") 'paredit-open-curly)
            (define-key nrepl-mode-map
              (kbd "}") 'paredit-close-curly)))
