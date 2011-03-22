(require 'ido)

(global-set-key [(ctrl tab)] 'ido-switch-buffer)
(global-set-key [(ctrl shift tab)] 'ido-switch-buffer)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-buffer-completion-map [(ctrl tab)] 'ido-next-match)
            (define-key ido-buffer-completion-map [(ctrl shift tab)] 'ido-prev-match)))

(ido-mode t)
(setq ido-enable-flex-matching t)

