;; check running on GUI system
(if (eq (symbol-value 'window-system) nil)
    (require 'highlight)
    (require 'facemenu+)
  (add-to-list 'load-path "~/.emacs.d/vendor/color-theme")
  (require 'color-theme)
  (color-theme-initialize)

  ;;(require 'zenburn)
  ;;(color-theme-zenburn)
  ;;(color-theme-gtk)
  ;;(require 'color-theme-arjen)
  ;;(color-theme-arjen)
  ;;(require 'color-theme-solarized)
  ;;(color-theme-solarized-light)
  ;;(color-theme-solarized-dark)

  (require 'color-theme-renndark)
  (color-theme-renndark)
  ;;(color-theme-vim-colors)
  )

(require 'face-list)
;; M-x list-faces-display