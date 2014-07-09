;; settings for maxframe(support fullscreen)
;; patched max-frame (for using Cocoa/Nextstep Emacs)
;; https://github.com/jmjeong/jmjeong-emacs/raw/master/vendor/maxframe.el
(add-to-list 'load-path "~/.emacs.d/vendor")
;;(load "~/.emacs.d/vendor/maxframe.el")
(require 'maxframe)
(defvar my-fullscreen-p t
  "check if fullscreen is on or off")

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (restore-frame)
    (maximize-frame)))
