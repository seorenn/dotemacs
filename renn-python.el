;; python-mode on launchpad
;; https://launchpad.net/python-mode

(require 'python-mode)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))

;; (setq python-mode-hook
;;       '(lambda () (progn
;;                     (set-variable 'py-indent-offset 4)
;;                     (set-variable 'py-smart-indentation nil)
;;                     (set-variable 'indent-tabs-mode nil)
;;                     (eldoc-mode 1))))

;; Pymacs + Ropemode

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)