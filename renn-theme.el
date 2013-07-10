(defun setup-terminal-theme ()
  (require 'color-theme)
  (require 'color-theme-solarized)
  (color-theme-solarized-dark))

(if (window-system)
    (load-theme 'solarized-dark t)
    (setup-terminal-theme))
