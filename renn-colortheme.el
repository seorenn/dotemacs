;; check running on GUI system
(unless (eq window-system nil)
  (progn
    (require 'facemenu+)
    (add-to-list 'load-path "~/.emacs.d/vendor/color-theme")
    (add-to-list 'load-path "~/.emacs.d/vendor/tomorrow-theme")
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

    ;;(require 'color-theme-renndark)
    ;;(color-theme-renndark)
    ;;(color-theme-vim-colors)

    (require 'color-theme-tomorrow)
    (color-theme-tomorrow-real 'night-bright)
    ))

(require 'face-list)
;; M-x list-faces-display