(defun setup-terminal-theme ()
  (require 'color-theme)
  (require 'color-theme-solarized)
  (color-theme-solarized-dark))

(defun renn-twilight ()
  (load-theme 'twilight t)
  ;; emacs-git-gutter
  (set-face-attribute 'git-gutter:modified nil :foreground "SlateBlue")
  (set-face-attribute 'git-gutter:added nil :foreground "Aquamarine")
  (set-face-attribute 'git-gutter:deleted nil :foreground "#CF6A4C")
  ;; flymake
  (set-face-attribute 'flymake-errline nil :background "#661111")
  )

(if (window-system)
    ;(load-theme 'solarized-dark t)
    (renn-twilight)
    (setup-terminal-theme))
