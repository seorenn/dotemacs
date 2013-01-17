(require 'facemenu+)

;; with Emacs 24
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;;(if window-system
;;    (require 'tomorrow-night-bright-theme))
;(require 'tomorrow-night-bright-theme)
(if window-system
    (require 'solarized-light-theme)
  (require 'tomorrow-night-bright-theme))

(require 'face-list)
;; M-x list-faces-display
