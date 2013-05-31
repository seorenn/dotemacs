(add-hook 'sgml-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 2)
            (sgml-guess-indent)))

;;;; php-mode
;; https://github.com/ejmr/php-mode

(require 'php-mode)

;;;; multi-web-mode
;; https://github.com/fgallina/multi-web-mode

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "html" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;;;; scss-mode
;; https://github.com/antonj/scss-mode

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(defun scss-custom ()
  "custom scss-mode-hook"
  (flymake-mode))
(add-hook 'scss-mode 'scss-custom)
