;;;; Cursor Movement

;; C-f / C-b: forward-char / backward-char
;; M-f / M-b: forward-word / backward-word
;; C-M-f / C-M-b: forward-sexp / backward-sexp

;; M-} / M-{: forward-paragraph / backward-paragraph

;; C-a / C-e: beginning-of-line / end-of-line
;; M-a / M-e: backward-sentence / forward-sentence
;; C-M-a / C-M-e: beginning-of-defun / end-of-defun

;; M-< / M->: beginning-of-buffer / end-of-buffer

;; C-n / C-p: next-line / previous-line

;; C-v / M-v: scroll-up / scroll-down

;;;; Universial Argument

;; C-u [COUNT] [KEY-COMMAND]: run KEY-COMMAND to COUNT times

(global-set-key (kbd "C->") 'other-window)
(global-set-key (kbd "C-<") 'other-window-reverse) ; in renn-func.el
;; (global-set-key (kbd "C-S-O") 'find-file-at-point)
(global-set-key (kbd "C-S-O") 'renn-open-url)
(global-set-key (kbd "M-\?") 'apropos)

(global-set-key (kbd "M-n") 'scroll-up-command)
(global-set-key (kbd "M-p") 'scroll-down-command)

(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-S-T") 'open-google-translate)

;; UNDO
(global-set-key (kbd "C-_") 'undo)
;;;; original C-z was bind suspend-emacs, but i don't need that.
(global-set-key (kbd "C-z") 'undo)

;; goto match paren
(global-set-key (kbd "C-%") 'goto-match-paren)

;; smart-beginning-of-line
(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key [home] 'smart-beginning-of-line)

;; from renn-dired.el
(global-set-key (kbd "C-x d") 'my-dired-home)

;; from renn-jumpchar.el
(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)

;; from renn-common.el
(add-hook 'objc-mode-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)))

;; from renn-expand-region.el
;(global-set-key (kbd "C-@") 'er/expand-region)

;; from renn-grep.el
(define-key global-map (kbd "C-x g") 'grep)

;; from renn-ibuffer.el
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; from renn-ido.el
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key [(ctrl tab)] 'ido-switch-buffer)
(global-set-key [(ctrl shift tab)] 'ido-switch-buffer)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-buffer-completion-map [(ctrl tab)] 'ido-next-match)
            (define-key ido-buffer-completion-map [(ctrl shift tab)] 'ido-prev-match)))

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; from renn-js.el
(defun coffee-custom-key ()
  "coffee-mode-hook for shortcuts"
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
  (define-key coffee-mode-map [(meta R)] 'coffee-compile-region))
(add-hook 'coffee-mode-hook 'coffee-custom-key)

;; from renn-korean.el
(global-set-key (kbd "S-SPC") 'toggle-input-method)
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
(global-set-key (kbd "<Hangul_Hanja>") 'hangul-to-hanja-conversion)

;; from renn-python.el
(defun python-custom-key ()
  "Customization for python.el mode"
  (define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-buffer-and-switch))
(add-hook 'python-mode-hook 'python-custom-key)

;; from renn-magit.el
(global-set-key (kbd "C-x G") 'magit-status)

;; from renn-maxframe.el
(global-set-key (kbd "M-RET") 'my-toggle-fullscreen)

;; from renn-moveregion.el
(global-set-key (kbd "M-P") 'move-line-region-up)
(global-set-key (kbd "M-N") 'move-line-region-down)

;; from renn-org.el
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-xa" 'org-agenda)
(add-hook 'org-load-hook
          (lambda ()
            (define-key org-mode-map "\C-co" 'renn-open-url)))

;;;; from renn-recentf.el

(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;; from renn-ruby.el

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "\C-m" 'newline-and-indent)))

;;;; from renn-search.el

(global-set-key (kbd "C-s") 'memory-and-search)
(global-set-key (kbd "C-r") 'memory-and-search-backward)
(global-set-key (kbd "C--") 'back-to-search-point)

(global-set-key (kbd "C-*") 'sr-highlight-symbol)
;; (global-set-key (kbd "C-*") 'sr-highlight-symbol-and-jump)
;; (global-set-key (kbd "M-*") 'sr-highlight-symbol-and-jump-prev)
(global-set-key (kbd "M-*") 'highlight-symbol-remove-all)
(global-set-key (kbd "C-M-*") 'highlight-symbol-remove-all)

;;;; from renn-shell.el

(global-set-key (kbd "C-x t") 'sh)
(defun m-eshell-hook ()
  (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line))
(add-hook 'eshell-mode-hook 'm-eshell-hook)

;;;; move window using meta-arrow

(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)

;;;; Helm

;(global-set-key (kbd "M-t") 'helm-for-files)
(global-set-key (kbd "M-t") 'helm-mini)
;(global-set-key (kbd "C-x p") 'helm-projectile)
;(global-set-key (kbd "M-o") 'helm-projectile)
(global-set-key (kbd "M-o") 'sr-open-file)
(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;;;; renn-speedbar

;(global-set-key (kbd "C-x p") 'my-speedbar)
;(global-set-key (kbd "C-x P") 'sr-speedbar-close)

;;;; neotree
(require 'neotree)
(global-set-key (kbd "C-x p") 'neotree-toggle)

;;;; shell-switcher

(global-set-key (kbd "M-'") 'shell-switcher-switch-buffer)
(global-set-key (kbd "C-x '") 'shell-switcher-switch-buffer)

;;;; visual-regexp

(global-set-key (kbd "C-c r") 'vr/replace)
(global-set-key (kbd "C-c q") 'vr/query-replace)
