(defun setup-terminal-theme ()
  (require 'color-theme)
  (require 'color-theme-solarized)
  (color-theme-solarized-dark)
  ;; Color Correction
  ;(set-face-attribute 'bg:erc-color-face0 nil :foreground "black")
  ;(set-face-attribute 'bg:erc-color-face15 nil :foreground "blue")
  (set-face-attribute 'column-marker-1 nil :foreground "black")
  (set-face-attribute 'eshell-ls-unreadable nil :foreground "red")
  (set-face-attribute 'header-line nil :foreground "black")
  (set-face-attribute 'highlight-indent-face nil :foreground "black")
  (set-face-attribute 'highlight-symbol-face nil :foreground "red")
  (set-face-attribute 'magit-log-head-label-tags nil :foreground "red")
  (set-face-attribute 'magit-log-reflog-label-cherry-pick nil :foreground "red")
  (set-face-attribute 'magit-log-reflog-label-commit nil :foreground "blue")
  (set-face-attribute 'magit-log-reflog-label-merge nil :foreground "cyan")
  (set-face-attribute 'mode-line nil :foreground "white" :background "navy")
  (set-face-attribute 'mode-line-inactive nil :foreground "black" :background "navy")
  )

(defun renn-eshell-with-twilight ()
  (set-face-attribute 'eshell-prompt nil :foreground "#CDA869")
  (set-face-attribute 'eshell-ls-executable nil :foreground "#CF6A4C")
  (set-face-attribute 'eshell-ls-symlink nil :foreground "SlateBlue")
  )

(defun renn-helm-with-twilight ()
  (set-face-attribute 'helm-selection nil :foreground "white" :background "grey20")
  (set-face-attribute 'helm-source-header nil
                      :font (face-attribute 'default :font)
                      :foreground "black"
                      :background "grey75" :weight 'bold)
  )

(defun renn-git-gutter-with-twilight ()
  (set-face-attribute 'git-gutter:added nil :foreground "Aquamarine")
  (set-face-attribute 'git-gutter:deleted nil :foreground "#CF6A4C")
  (set-face-attribute 'git-gutter:modified nil :foreground "#8F9D6A")
  )

(defun renn-twilight ()
  (load-theme 'twilight t)
  ;; hl-line
  (set-face-background 'hl-line "#222")
  ;; flymake
  (set-face-attribute 'flymake-errline nil :background "#661111")
  ;; eshell
  (add-hook 'eshell-mode-hook 'renn-eshell-with-twilight)
  ;; helm
  (add-hook 'helm-after-initialize-hook 'renn-helm-with-twilight)
  ;; git-gutter
  (renn-git-gutter-with-twilight)
  )

(if (window-system)
    ;(load-theme 'solarized-dark t)
    (renn-twilight)
   (setup-terminal-theme))
