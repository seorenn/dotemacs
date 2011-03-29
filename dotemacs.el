;; USAGE
;; 1) Create ~/.emacs, write these:
;;    (add-to-list 'load-path "~/.emacs.d")
;;    (load-library "dotemacs")
;; 2) or Link dotemacs.el to ~/.emacs
;;    (Here's were some problem what emacs over-writtable .emacs)

(add-to-list 'load-path "~/.emacs.d")
(progn (cd "~/.emacs.d")
       (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/vendor")

;; load default functions
(load-library "renn-func")

;; load private variables
(load-my-library "renn-private")

;; load configurations :)
(load-my-library "renn-encoding")
(load-my-library "renn-korean")
(load-my-library "renn-common")
(load-my-library "renn-gui")
(load-my-library "renn-ido")
(load-my-library "renn-autoinstall")
(load-my-library "renn-shell")
(load-my-library "renn-simplenote")
(load-my-library "renn-c")
(load-my-library "renn-autocomplete")
(load-my-library "renn-colortheme")
(load-my-library "renn-vim")
(load-my-library "renn-org")
(load-my-library "renn-markdown")
(load-my-library "renn-anything")

(if (file-exists-p "~/.emacs.d/vendor/pymacs.el")
    (load-my-library "renn-python"))

(if (file-exists-p "~/.emacs.d/vendor/twittering-mode/twittering-mode.el")
    (load-my-library "renn-twitter"))

;; system-type: gnu, gnu/linux, darwin, ms-dos, windows-nt, cygwin, ...
(if (eq system-type 'darwin)
    (load-library "renn-osx"))
(if (eq system-type 'windows-nt)
    (load-library "renn-win32"))
(if (eq system-type 'gnu/linux)
    (load-library "renn-linux"))
