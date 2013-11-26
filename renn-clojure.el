(require 'clojure-mode)
;(require 'nrepl)
(require 'ac-nrepl)
(require 'align-cljlet)
(require 'cider)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(setq nrepl-hide-special-buffers t)
(setq cider-repl-tab-command 'indent-for-tab-command)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq nrepl-buffer-name-separator "-")
(setq nrepl-buffer-name-show-port t)
(setq cider-repl-display-in-current-window t)

;; (setq nrepl-popup-stacktraces-in-repl nil)
;; (add-to-list 'same-window-buffer-names "*nrepl*")
;; (setq nrepl-history-file "~/.emacs.d/nrepl-history")
;; (setq nrepl-popup-stacktraces nil)

;; (add-hook 'nrepl-connected-hook
;;           (defun renn-clojure-mode-eldoc-hook ()
;;             (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
;;             (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
;;             (nrepl-enable-on-existing-clojure-buffers)))

;; (add-hook 'nrepl-mode-hook 'subword-mode)

;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode))
;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)

;; (add-hook 'nrepl-interaction-mode-hook
;;           (lambda ()
;;             (nrepl-turn-on-eldoc-mode)
;;             (enable-paredit-mode)
;;             (define-key nrepl-mode-map
;;               (kbd "{") 'paredit-open-curly)
;;             (define-key nrepl-mode-map
;;               (kbd "}") 'paredit-close-curly)))
