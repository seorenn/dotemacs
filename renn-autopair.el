(add-to-list 'load-path "~/.emacs.d/vendor")

(require 'autopair)
(autopair-global-mode 1)

(setq autopair-autowrap t)
(add-hook 'python-mode-hook
          '(lambda ()
             (setq autopair-handle-action-fns
                   (list 'autopair-default-handle-action
                         'autopair-python-triple-quote-action))))

(provide 'renn-autopair)