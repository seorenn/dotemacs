(require 'speedbar)

;; Hidden

(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")

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

(speedbar-add-supported-extension ".wsgi")

;; etc

(setq speedbar-frame-parameters '((minibuffer)
                                  (width . 40)
                                  (border-width . 0)
                                  (menu-bar-lines . 0)
                                  (tool-bar-lines . 0)
                                  (unsplittable . t)
                                  (left-fringe . 0)))
(setq speedbar-use-images nil)
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-smart-directory-expand-flag t)

;;;; sr-speedbar.el
;; http://www.emacswiki.org/emacs/SrSpeedbar

(require 'sr-speedbar)

(setq sr-speedbar-right-side nil)
(setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-max-width 70)
(setq sr-speedbar-width-console 40)
(setq sr-speedbar-skip-other-window-p t)

(when window-system
  (defadvice sr-speedbar-open (after sr-speedbar-open-resize-frame activate)
    (set-frame-width (selected-frame)
                     (+ (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-open 'after 'sr-speedbar-open-resize-frame)

  (defadvice sr-speedbar-close (after sr-speedbar-close-resize-frame activate)
    (sr-speedbar-recalculate-width)
    (set-frame-width (selected-frame)
                     (- (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-close 'after 'sr-speedbar-close-resize-frame))

(defun my-speedbar ()
  "Toggle sr-speedbar and select"
  (interactive)
  (progn
    (unless (sr-speedbar-exist-p)
      (sr-speedbar-open))
    (sr-speedbar-select-window)))

(provide 'renn-speedbar)
