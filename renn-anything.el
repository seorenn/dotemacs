(require 'anything-startup)

;; index file for anything-filelist+
;;(setq anything-c-filelist-file-name "~/.myfiles-index")

(global-set-key "\C-x\C-f" 'anything-find-file)
(global-set-key "\C-x\C-a\C-f" 'anything-for-files)
(global-set-key [(ctrl tab)] 'anything-for-buffers)


