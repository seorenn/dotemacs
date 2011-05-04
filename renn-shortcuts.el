(global-set-key (kbd "C->") 'other-window)
(global-set-key (kbd "C-<") 'other-window-reverse) ; in renn-func.el
;; (global-set-key (kbd "C-S-O") 'find-file-at-point)
(global-set-key (kbd "C-S-O") 'renn-open-url)
(global-set-key (kbd "M-\?") 'apropos)
;; original C-z was bind suspend-emacs, but i don't need that.
(global-set-key (kbd "C-z") 'undo)

;; move window using meta-arrow
(windmove-default-keybindings 'meta)

(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-S-T") 'open-google-translate)

;; dired
(defun my-dired-home ()
  (interactive)
  (dired "~"))

(global-set-key (kbd "C-x d") 'my-dired-home)

;; goto match paren
(global-set-key (kbd "C-%") 'goto-match-paren)