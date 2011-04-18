(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Aquamacs options
(setq inhibit-startup-echo-area-message t)
;;(aquamacs-autoface-mode nil)
;;(tabbar-mode nil)
;;(one-buffer-one-frame-mode 0)

;(set-face-font 'default "Monaco-11")
(set-face-font 'default "-apple-Monaco-medium-normal-normal-*-12-120-72-72-m-120-iso10646-1")

;; Korean
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                  "-apple-AppleGothic-medium-normal-normal-*-14-120-72-72-p-120-*-*")

;; Unicode User Area
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                  "-apple-Hiragino_Kaku_Gothic_Pro-medium-normal-normal-*-14-120-72-72-p-120-iso10646-1")
;                  "-apple-AppleGothic-medium-normal-normal-*-14-120-72-72-p-120-*-*")

;; Japanese & Chinese
;; (set-fontset-font "fontset-default" 'kana "Hiragino Kaku Gothic Pro-14")
;; (set-fontset-font "fontset-default" 'han "Hiragino Kaku Gothic Pro-14")
(set-fontset-font "fontset-default" 'kana
                  "-apple-Hiragino_Kaku_Gothic_Pro-medium-normal-normal-*-14-120-72-72-p-120-iso10646-1")
(set-fontset-font "fontset-default" 'han
                  "-apple-Hiragino_Kaku_Gothic_Pro-medium-normal-normal-*-14-120-72-72-p-120-iso10646-1-*")