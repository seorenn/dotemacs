;; USAGE
;; 1) Create ~/.emacs, write these:
;;    (add-to-list 'load-path "~/.emacs.d")
;;    (load-library "dotemacs")
;; 2) or Link dotemacs.el to ~/.emacs
;;    (Here's were some problem what emacs over-writtable .emacs)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/vendor")

;; system-type: gnu, gnu/linux, darwin, ms-dos, windows-nt, cygwin, ...
(cond
 ((eq system-type 'darwin) (load-library "renn-osx"))
 ((eq system-type 'windows-nt) (load-library "renn-win32"))
 ((eq system-type 'gnu/linux) (load-library "renn-linux")))

;; load default functions
(load-library "renn-func")

;; load private variables
(load-my-library "renn-private")

;; load my custom extensions
(load-my-library "renn-postform")
(load-my-library "renn-explorefile")
(load-my-library "renn-moveregion")

;; load other vendor's functions
;;(load-my-library "renn-lively")

;; load configurations :)
(load-my-library "renn-encoding")
(load-my-library "renn-korean")
(load-my-library "renn-common")
(load-my-library "renn-grep")
(load-my-library "renn-search")
(load-my-library "renn-autopair")
(load-my-library "renn-ibuffer")
(load-my-library "renn-uniquify")
(load-my-library "renn-twitter")
(load-my-library "renn-gui")
(load-my-library "renn-ido")
(load-my-library "renn-autoinstall")
(load-my-library "renn-shell")
(load-my-library "renn-devel")
(load-my-library "renn-css")
(load-my-library "renn-python")
(load-my-library "renn-html")
(load-my-library "renn-js")
(load-my-library "renn-magit")
(load-my-library "renn-yasnippet")
(load-my-library "renn-autocomplete")
(load-my-library "renn-org")
(load-my-library "renn-markdown")
(load-my-library "renn-zencoding")
(load-my-library "renn-maxframe")
(load-my-library "renn-colortheme")
(load-my-library "renn-shortcuts")
(load-my-library "renn-speedbar")
(load-my-library "renn-calfw")

(cd "~/")

(eshell)