(add-hook 'sgml-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 2)
            (sgml-guess-indent)))

;;;; multi-web-mode
;; https://github.com/fgallina/multi-web-mode

(add-to-list 'load-path "~/.emacs.d/vendor/multi-web-mode")
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "html" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)
