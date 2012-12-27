;; Mac OS X Encodings

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; New-line
(set-buffer-file-coding-system 'mac)

;; Command key is meta key
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(setq inhibit-startup-echo-area-message t)

(unless (eq window-system nil)
  ;; Enable Anti-Aliases
  ;(setq mac-allow-anti-aliasing t)
  ;(setq ns-antialias-text t)

  ;; Main Font Face
  ;(set-face-font 'default "Monaco-12")
  ;(set-face-attribute 'default nil :font "Monaco" :height 120)
  ;(set-face-font 'default "Monaco-12")

  (set-face-font 'default "Menlo-12")
  ;(set-face-attribute 'default nil :font "Menlo-12")
  ;(set-face-attribute 'default nil :family "Menlo" :height 120)
  (setq-default line-spacing 2)

  ;; Korean Unicode SPEC.
  ;; EUC-KR(cp949)은 조잡한 미완성 한글 스펙입니다.
  ;; 쓰지맙시다. 윈도우를 죽입시다. 나쁜 윈도우!

  ;; 나눔고딕(네이버 나눔글꼴 패키지)을 기본 한글 서체로 사용.
  (set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                    '("Apple SD Gothic Neo" . "iso10646-1"))

  ;; Unicode User Area
  (set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                    '("Apple SD Gothic Neo" . "iso10646-1"))

  (set-fontset-font t 'hangul (font-spec :name "Apple SD Gothic Neo"))

  ;; Japanese & Chinese
  ;; I don't know Chinese. So configured all type of Chinese/Kanji/Han to Japanese Font Spec
  ;; 일본어나 특수 한자를 쓰지 않는다면 이 설정은 의미 없을지도 모른답니다. ;)
  (set-fontset-font "fontset-default" 'kana
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'han
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'japanese-jisx0208
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'japanese-jisx0213.2004-1
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  (set-fontset-font "fontset-default" 'japanese-jisx0213-2
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  ;; half-width katakana
  (set-fontset-font "fontset-default" 'katakana-jisx0201
                    '("Hiragino Kaku Gothic Pro" . "iso10646-1"))

  ;(setq fixed-width-use-QuickDraw-for-ascii t) ; is need this? ;)
  ;(setq mac-allow-anti-aliasing t)             ; OK! I hate bitmap font

  ;; Options for scales each font
  ;; 각 폰트 사이의 크기를 기준폰트(default-font) 배수로 설정
  (setq face-font-rescale-alist
        '((".*hiragino.*" . 1.2)
          (".*sd.*gothic.*" . 1.4))))

;; Copy/Paste Integration for Emacs in Terminal

(when (eq window-system nil)
  (defun mac-copy ()
    (shell-command-to-string "pbpaste"))

  (defun mac-paste (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'mac-paste)
  (setq interprogram-paste-function 'mac-copy))
