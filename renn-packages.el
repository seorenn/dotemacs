(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(setq package-list
 '(ac-nrepl
   align-cljlet
   android-mode
   auto-complete
   autopair
   cider
   clojure-mode
   coffee-mode
   color-theme
   cyberpunk-theme
   ;dash
   epl
   expand-region
   flymake
   flymake-cursor
   git-gutter
   helm
   helm-c-yasnippet
   helm-cmd-t
   helm-git
   helm-projectile
   helm-swoop
   highlight-symbol
   ;ipython
   js2-mode
   js3-mode
   jump-char
   magit
   markdown-mode
   maxframe
   paredit
   popup
   projectile
   quickrun
   rainbow-delimiters
   s
   scss-mode
   shell-switcher
   smartparens
   solarized-theme
   sr-speedbar
   switch-window
   visual-regexp
   visual-regexp-steroids
   web-mode
   yasnippet))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))
