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

(load-library "renn-func")
(load-library "renn-encoding")
(load-library "renn-korean")
(load-library "renn-common")
(load-library "renn-ido")
(load-library "renn-autoinstall")
(load-library "renn-shell")

;; system-type: gnu, gnu/linux, darwin, ms-dos, windows-nt, cygwin, ...
(if (eq system-type 'darwin)
    (load-library "renn-osx"))
(if (eq system-type 'windows-nt)
    (load-library "renn-win32"))
(if (eq system-type 'gnu/linux)
    (load-library "renn-linux"))
