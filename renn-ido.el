(require 'ido)

(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key [(ctrl tab)] 'ido-switch-buffer)
(global-set-key [(ctrl shift tab)] 'ido-switch-buffer)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-buffer-completion-map [(ctrl tab)] 'ido-next-match)
            (define-key ido-buffer-completion-map [(ctrl shift tab)] 'ido-prev-match)))

(setq ido-confirm-unique-completion t)
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'samewindow)
(setq ido-use-filename-at-point t)
;; (setq ido-work-directory-list
;;       '("~/.emacs.d"
;;         "~/Dropbox/notes"
;;         "~/Devel/apiserver"))
(setq ido-max-work-directory-list 100)
(setq ido-max-work-file-list 20)

(ido-mode t)
(ido-everywhere t)
(icomplete-mode t)

(set-face-foreground 'ido-first-match "white")
(set-face-background 'ido-first-match "SaddleBrown")