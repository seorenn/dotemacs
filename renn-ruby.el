;;;; for Ruby Mode

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda ()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace)
                           )))
            (set (make-local-variable 'indent-tabs-mode) nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            ;(define-key ruby-mode-map "\C-m" 'newline-and-indent)
            ))
