;; This settings needs these fonts: Bitstrea Vera Sans Mono + NanumGothic(나눔고딕)
;; 세로크기가 안맞아서 한글이 포함되면 영문의 라인 사이 간격이 벌어집니다.
;; 하지만 설정하기가 매우 귀찮아요. 하하하...

(set-face-font 'default "Bitstream Vera Sans Mono-9")
(set-fontset-font "fontset-default" '(#x1100 . #xffdc) "NanumGothic-11")
(set-fontset-font "fontset-default" 'kana "Sazanami Gothic-11")
(set-fontset-font "fontset-default" 'han "Sazanami Gothic-11")