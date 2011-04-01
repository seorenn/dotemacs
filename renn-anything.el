(require 'anything-startup)

(global-set-key "\C-x\C-f" 'anything-for-files)
(global-set-key [(ctrl tab)] 'anything-for-buffers)

; bypass troubles with ido
;(anything-read-string-mode nil)