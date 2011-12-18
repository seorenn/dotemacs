(add-hook 'sgml-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 2)
            (sgml-guess-indent)))

;; html-helper-mode
(add-to-list 'load-path "~/.emacs.d/vendor/html-helper-mode")
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.phpl$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.asp$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jsp$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.phtml$" . html-helper-mode) auto-mode-alist))
