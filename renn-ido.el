(require 'ido)

(setq ido-confirm-unique-completion t)
(setq ido-enable-flex-matching t)
(setq ido-max-work-directory-list 150)
(setq ido-max-work-file-list 30)

(ido-mode t)
(ido-everywhere t)
(icomplete-mode t)
