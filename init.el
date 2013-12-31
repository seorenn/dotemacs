(setq debug-on-error nil)               ; put t if you want debug elisp

(add-to-list 'load-path "~/.emacs.d")
;; (add-to-list 'load-path "~/.emacs.d/elpa")
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
(load-my-library "renn-packages")

;; load private variables
(load-my-library "renn-private")

;; load my custom extensions
(load-my-library "renn-postform")
(load-my-library "renn-explorefile")
(load-my-library "renn-moveregion")

;; load configurations
(load-my-library "renn-gui")
(load-my-library "renn-encoding")
(load-my-library "renn-korean")
(load-my-library "renn-common")
(load-my-library "renn-grep")
(load-my-library "renn-quickrun")
(load-my-library "renn-search")
;(load-my-library "renn-autopair")
(load-my-library "renn-smartparens")
;(load-my-library "renn-ibuffer")
;(load-my-library "renn-uniquify")
(load-my-library "renn-ido")
;(load-my-library "renn-autoinstall")
(load-my-library "renn-shell")
(load-my-library "renn-paredit")
(load-my-library "renn-devel")
(load-my-library "renn-autocomplete")
(load-my-library "renn-python")
(load-my-library "renn-ruby")
(load-my-library "renn-js")
(load-my-library "renn-clojure")
(load-my-library "renn-webdevel")
(load-my-library "renn-yasnippet")
;(load-my-library "renn-company")
(load-my-library "renn-git")
;(load-my-library "renn-expand-region")
(load-my-library "renn-org")
;(load-my-library "renn-dired")
;(load-my-library "renn-jumpchar")
(load-my-library "renn-switch-window")
(load-my-library "renn-markdown")
;(load-my-library "renn-zencoding")
(load-my-library "renn-speedbar")
;(load-my-library "renn-maxframe")
(load-my-library "renn-theme")
(load-my-library "renn-term")
(load-my-library "renn-helm")
(load-my-library "renn-projectile")
;(load-my-library "renn-modeline")

(load-my-library "renn-iterm2func")

;; LAST: reassign keymaps
(load-my-library "renn-shortcuts")

(cd "~/")
