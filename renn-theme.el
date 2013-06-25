;; with Emacs 24
;(load-theme 'cyberpunk t)
;(color-theme-sanityinc-tomorrow-bright)
;(require 'solarized)

;;(require 'solarized-dark-theme)
(unless window-system
    (setq solarized-use-terminal-theme t)
    (setq solarized-termcolor 256)
    (setq solarized-degrade t)
    ;; (setq solarized-bold nil)
    ;; (setq solarized-underline nil)
    ;; (setq solarized-italic nil)
    ;; (setq solarized-contrast 'high)
    ;; (setq solarized-visibility 'high)
    ;; (setq solarized-broken-srgb nil)
    )

(load-theme 'solarized-dark t)
;; (when window-system
;;   (load-theme 'solarized-dark t))

;(load-theme solarized-dark-theme t)
;(require 'sanityinc-tomorrow-bright-theme)
;(require 'color-theme-sanityinc-tomorrow-bright)

;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;(add-to-list 'load-path "~/.emacs.d/themes")
