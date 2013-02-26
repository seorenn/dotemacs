(require 'autopair)

;;(autopair-global-mode 1)

(defun turn-on-autopair ()
  (autopair-mode 1))

(add-hook 'lisp-mode-hook 'turn-on-autopair)
(add-hook 'emacs-lisp-mode-hook 'turn-on-autopair)
(add-hook 'c-mode-hook 'turn-on-autopair)
(add-hook 'c++-mode-hook 'turn-on-autopair)
(add-hook 'python-mode-hook 'turn-on-autopair)
;(add-hook 'js-mode-hook 'turn-on-autopair)
(add-hook 'js2-mode-hook 'turn-on-autopair)
(add-hook 'java-mode-hook 'turn-on-autopair)

(setq autopair-autowrap t)
(add-hook 'python-mode-hook
          '(lambda ()
             (setq autopair-handle-action-fns
                   (list 'autopair-default-handle-action
                         'autopair-python-triple-quote-action))))

(provide 'renn-autopair)
