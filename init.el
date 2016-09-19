
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq debug-on-error nil)               ; put t if you want debug elisp

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/vendor")

;; system-type: gnu, gnu/linux, darwin, ms-dos, windows-nt, cygwin, ...
(cond
 ((eq system-type 'darwin) (load-library "renn-osx"))
 ((eq system-type 'windows-nt) (load-library "renn-win32"))
 ((eq system-type 'gnu/linux) (load-library "renn-linux")))

;; set the path as terminal path [http://lists.gnu.org/archive/html/help-gnu-emacs/2011-10/msg00237.html]
(setq explicit-bash-args (list "--login" "-i"))

;; fix the PATH variable for GUI [http://clojure-doc.org/articles/tutorials/emacs.html#osx]
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -l -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; load default functions
(load-library "renn-func")

;; load default package list and install if not installed
;(load-library "renn-packages")
(load-library "renn-cask")

;; load private variables
(load-library "renn-private")

;; load my custom extensions
(load-library "renn-postform")
(load-library "renn-explorefile")
(load-library "renn-moveregion")

;; load configurations
(load-library "renn-gui")
(load-library "renn-encoding")
(load-library "renn-korean")
(load-library "renn-common")
(load-library "renn-grep")
(load-library "renn-quickrun")
(load-library "renn-search")
;(load-library "renn-autopair")
(load-library "renn-smartparens")
;(load-library "renn-ibuffer")
;(load-library "renn-uniquify")
(load-library "renn-ido")
;(load-library "renn-autoinstall")
(load-library "renn-shell")
;(load-library "renn-paredit")
(load-library "renn-devel")
(load-library "renn-autocomplete")
(load-library "renn-swift")
(load-library "renn-python")
(load-library "renn-ruby")
(load-library "renn-js")
(load-library "renn-clojure")
(load-library "renn-webdevel")
(load-library "renn-yasnippet")
;(load-library "renn-company")
;(load-library "renn-expand-region")
(load-library "renn-org")
;(load-library "renn-dired")
;(load-library "renn-jumpchar")
(load-library "renn-switch-window")
(load-library "renn-markdown")
;(load-library "renn-zencoding")
;(load-library "renn-speedbar")
;(load-library "renn-maxframe")
(load-library "renn-theme")
(load-library "renn-git")
(load-library "renn-term")
(load-library "renn-helm")
(load-library "renn-projectile")
(load-library "renn-neotree")
;(load-library "renn-modeline")
(load-library "renn-evil")

;(load-my-library "renn-iterm2func")

;; LAST: reassign keymaps
(load-library "renn-shortcuts")

(cd "~/")
