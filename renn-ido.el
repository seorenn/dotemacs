(require 'ido)

(global-set-key [(ctrl tab)] 'ido-switch-buffer)
(global-set-key [(ctrl shift tab)] 'ido-switch-buffer)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-buffer-completion-map [(ctrl tab)] 'ido-next-match)
            (define-key ido-buffer-completion-map [(ctrl shift tab)] 'ido-prev-match)))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-work-directory-list
      '("~/.emacs.d"
        "~/Dropbox/notes"
        "~/Devel/apiserver"))
(setq ido-max-work-directory-list 100)
(setq ido-max-work-file-list 1000)
(ido-mode t)
