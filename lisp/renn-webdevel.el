(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq web-mode-indent-style 1)
(setq web-mode-comment-style 2)

(setq web-mode-style-padding 0)
(setq web-mode-script-padding 0)
(setq web-mode-block-padding 0)

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-column-highlight t)

(setq web-mode-engines-alist
      '(("php" . "\\.phtml\\'")
        ("jinja" . "\\.html\\'")))

;;;; scss-mode
;;
;; Install compass with GEM
;; $ gem install compass
;;
;; Where is compass installing?
;; $ gem which compass

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(defun scss-custom ()
  "custom scss-mode-hook"
  (flymake-mode))
(add-hook 'scss-mode 'scss-custom)
