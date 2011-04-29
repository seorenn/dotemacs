(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq inhibit-startup-echo-area-message t)

;; Aquamacs specific options ---------
;; (aquamacs-autoface-mode nil)
;; (tabbar-mode nil)
;; (one-buffer-one-frame-mode 0)

;; Font Set -----------
;; Hint) 1. Select font by Menu [Options - Set Default Font] - (select you wanted)
;;       2. Move point to some character, then press C-u C-x =
;;          (or command M-x what-cursor-position)
;;       3. Emasc will display font-spec in splited window (buffer named *Help*)
;;
;; Tools) M-x anything-select-xfont will listing fonts list, but not all. :(

;; 의미없는 설정인 듯...
;; (add-to-list 'default-frame-alist '(font . "fontset-default"))

(set-face-font 'default "Monaco-12")

;; Korean Unicode SPEC.
;; EUC-KR(cp949)은 조잡한 미완성 한글 스펙입니다.
;; 쓰지맙시다. 윈도우를 죽입시다. 나쁜 윈도우!

;; Mac OS X 기본 글꼴인 애플고딕 설정 예. 꼭 X-Font 스타일로 적을 필요는 없음.
;; (set-fontset-font "fontset-default" '(#x1100 . #xffdc)
;;                   "-apple-AppleGothic-medium-normal-normal-*-14-120-72-72-p-120-*-*")

;; 나눔고딕(네이버 나눔글꼴 패키지)을 기본 한글 서체로 사용.
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                  '("NanumGothicOTF" . "iso10646-1"))

;; Unicode User Area
;; (set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
;;                   "-apple-Hiragino_Kaku_Gothic_Pro-medium-normal-normal-*-14-120-72-72-p-120-iso10646-1")

(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                  '("NanumGothicOTF" . "iso10646-1"))

;; Japanese & Chinese
;; I don't know Chinese. So configured all type of Chinese/Kanji/Han to Japanese Font Spec
;; 일본어나 특수 한자를 쓰지 않는다면 이 설정은 의미 없을지도 모른답니다. ;)

;; (set-fontset-font "fontset-default" 'kana "Hiragino Kaku Gothic Pro-14")
;; (set-fontset-font "fontset-default" 'han "Hiragino Kaku Gothic Pro-14")
;; (set-fontset-font "fontset-default" 'kana
;;                   "-apple-Hiragino_Kaku_Gothic_Pro-medium-normal-normal-*-14-120-72-72-p-120-iso10646-1")
;; (set-fontset-font "fontset-default" 'han
;;                   "-apple-Hiragino_Kaku_Gothic_Pro-medium-normal-normal-*-14-120-72-72-p-120-iso10646-1-*")

(set-fontset-font "fontset-default" 'kana
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
(set-fontset-font "fontset-default" 'han
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
(set-fontset-font "fontset-default" 'japanese-jisx0208
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
(set-fontset-font "fontset-default" 'katakana-jisx0201
                  '("Hiragino Kaku Gothic Pro" . "iso10646-1"))

(setq fixed-width-use-QuickDraw-for-ascii t) ; is need this? ;)
(setq mac-allow-anti-aliasing t)             ; OK! I hate bitmap font

;; Options for scales each font
;; 각 폰트 사이의 크기를 기준폰트(default-font) 배수로 설정
(setq face-font-rescale-alist
      '((".*hiragino.*" . 1.2)
        (".*nanum.*" . 1.3)))

;; settings for maxframe(support fullscreen)
;; patched max-frame (for using Cocoa/Nextstep Emacs)
;; https://github.com/jmjeong/jmjeong-emacs/raw/master/vendor/maxframe.el
(require 'maxframe)
(defvar my-fullscreen-p t
  "check if fullscreen is on or off")

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (restore-frame)
    (maximize-frame)))

(global-set-key (kbd "M-RET") 'my-toggle-fullscreen)