;; renn-devel.el
;; This files set the Environment for Software Develomentation to Emacs

;; flymake mode
(require 'flymake)
(require 'flymake-cursor)

;; ----- C

(setq-default c-basic-offset 4
              indent-tabs-mode nil)

;; ----- Objective-C

(setq auto-mode-alist (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mm$" . objc-mode) auto-mode-alist))

;; ----- Java and Android

(require 'android-mode)

;; Refered from:
;; https://github.com/jmjeong/jmjeong-emacs/blob/master/jmjeong-objc.el
(defun my-header-file-mode-hook ()
  (if (string-equal (file-name-extension buffer-file-name) "h")
          (let ((filebase (file-name-sans-extension buffer-file-name)))
                (cond
                 ((file-exists-p (concat filebase ".c"))
                  (c-mode))
                 ((file-exists-p (concat filebase ".cpp"))
                  (c++-mode))
                 ((file-exists-p (concat filebase ".cc"))
                  (c++-mode))
                 ((file-exists-p (concat filebase ".m"))
                  (objc-mode))
                 ((file-exists-p (concat filebase ".mm"))
                  (objc-mode))
                 (t
                  (objc-mode))))))

(add-hook 'find-file-hook 'my-header-file-mode-hook)
