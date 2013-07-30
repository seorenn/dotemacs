(defun setup-terminal-theme ()
  (require 'color-theme)
  (require 'color-theme-solarized)
  (color-theme-solarized-dark))

(defun renn-eshell-with-twilight ()
  (set-face-attribute 'eshell-prompt nil :foreground "#CDA869")
  (set-face-attribute 'eshell-ls-executable nil :foreground "#CF6A4C")
  (set-face-attribute 'eshell-ls-symlink nil :foreground "SlateBlue")
  )

(defun renn-twilight ()
  (load-theme 'twilight t)
  ;; flymake
  (set-face-attribute 'flymake-errline nil :background "#661111")
  (set-face-attribute 'helm-selection nil :foreground "white" :background "grey20")
  (set-face-attribute 'helm-source-header nil
                      :font (face-attribute 'default :font)
                      :foreground "black"
                      :background "grey75" :weight 'bold)
  ;; eshell
  (add-hook 'eshell-mode-hook 'renn-eshell-with-twilight)
  )

(if (window-system)
    ;(load-theme 'solarized-dark t)
    (renn-twilight)
    (setup-terminal-theme))
