(require 'hideshow)
(setq hs-minor-mode-hook nil)
(dynamic-completion-mode)

(setq next-line-add-newlines nil)
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq hscroll-step 1)
(setq scroll-conservatively 10000)
(setq sentence-end-double-space nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq make-backup-files nil)
(setq next-line-add-newlines nil)
(setq track-eol nil)
(setq default-tab-width 4)
(setq isearch-allow-scroll t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq-default case-fold-search t)

(delete-selection-mode nil)
(which-function-mode t)
(global-font-lock-mode t)
(transient-mark-mode t)
(show-paren-mode t)
(line-number-mode 1)
(column-number-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist '(cursor-type . 'box))

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)
;(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-N") 'other-window)
;(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-P") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-O") 'find-file-at-point)
(global-set-key (kbd "M-O") 'find-file)
(global-set-key (kbd "M-S") 'save-buffer)
(global-set-key (kbd "M-?") 'apropos)

(define-key global-map (kbd "RET") 'newline-and-indent)






