(require 'ido)

(setq ido-confirm-unique-completion t)
(setq ido-enable-flex-matching t)
;;(setq ido-default-buffer-method 'samewindow)
;;(setq ido-use-filename-at-point t)
;; (setq ido-work-directory-list
;;       '("~/.emacs.d"
;;         "~/Dropbox/notes"
;;         "~/Devel/apiserver"))
(setq ido-max-work-directory-list 150)
(setq ido-max-work-file-list 30)

(ido-mode t)
(ido-everywhere t)
(icomplete-mode t)

;; (set-face-foreground 'ido-first-match "white")
;; (set-face-background 'ido-first-match "SaddleBrown")

; SMEX - Smart M-x Enhancer (require IDO)
(require 'smex)
(smex-initialize)
