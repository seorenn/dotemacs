;; Some file extensions for speedbar showing

(speedbar-add-supported-extension ".c")
(speedbar-add-supported-extension ".cc")
(speedbar-add-supported-extension ".c++")
(speedbar-add-supported-extension ".h")

(speedbar-add-supported-extension ".m")
(speedbar-add-supported-extension ".mm")

(speedbar-add-supported-extension ".sh")

(speedbar-add-supported-extension ".py")

(speedbar-add-supported-extension ".pl")

(speedbar-add-supported-extension ".ruby")

(speedbar-add-supported-extension ".xml")
(speedbar-add-supported-extension ".htm")
(speedbar-add-supported-extension ".html")
(speedbar-add-supported-extension ".css")

(speedbar-add-supported-extension ".php")

(speedbar-add-supported-extension ".txt")
(speedbar-add-supported-extension ".org")
(speedbar-add-supported-extension ".md")
(speedbar-add-supported-extension ".mdown")
(speedbar-add-supported-extension ".markdown")

(speedbar-add-supported-extension ".java")
(speedbar-add-supported-extension ".js")
(speedbar-add-supported-extension ".coffee")
(speedbar-add-supported-extension ".scss")

;;;; sr-speedbar.el
;; http://www.emacswiki.org/emacs/SrSpeedbar

(require 'sr-speedbar)

(setq sr-speedbar-right-side nil)
(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-width-x 400)

(defun my-speedbar ()
  "Toggle sr-speedbar and select"
  (interactive)
  (progn
    (sr-speedbar-toggle)
    (if (sr-speedbar-exist-p)
        (sr-speedbar-select-window))))

(global-set-key (kbd "C-x p") 'my-speedbar)
