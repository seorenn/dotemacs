;; (require 'anything-startup)

;; index file for anything-filelist+
;;(setq anything-c-filelist-file-name "~/.myfiles-index")

;; (global-set-key "\C-x\C-f" 'anything-find-file)
;; (global-set-key "\C-x\C-a\C-f" 'anything-for-files)
;; (global-set-key [(ctrl tab)] 'anything-for-buffers)

(require 'anything)
(require 'anything-config)

(setq anything-sources
      '(anything-c-source-buffers
        anything-c-source-buffer-not-found
        anything-c-source-file-name-history
        anything-c-source-file-cache))

(global-set-key (kbd "M-+") 'anything)