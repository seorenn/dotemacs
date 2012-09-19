;;;; Configurations for JavaScript Editing ;;;;
;;;; with js2-mode

(add-to-list 'load-path "~/.emacs.d/vendor/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Configurations for js3-mode
;; (add-to-list 'load-path "~/.emacs.d/vendor/js3-mode")
;; (autoload 'js3-mode "js3" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
;; (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))


;; (setq js3-lazy-commas t)
;; (setq js3-lazy-operators t)
;; (setq js3-lazy-dots t)
;; (setq js3-expr-indent-offset 2)
;; (setq js3-paren-indent-offset 2)
;; (setq js3-square-indent-offset 2)
;; (setq js3-curly-indent-offset 2)
;; ;(setq js3-indent-level 2)
;; (setq js3-auto-indent-p t)
;; (setq js3-enter-indents-newline t)
;; (setq js3-indent-on-enter-key t)

;; jade-mode
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;;;; Configurations for Coffee-Script ;;;;
;;https://github.com/defunkt/coffee-mode

(add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2)

  (setq coffee-args-compile '("-c" "--bare"))

  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
  (define-key coffee-mode-map [(meta R)] 'coffee-compile-region)

  ;(and (file-exists-p (buffer-file-name))
  ;     (file-exists-p (coffee-compiled-file-name))
  ;     (coffee-cos-mode t))
  (coffee-cos-mode t))

(add-hook 'coffee-mode-hook 'coffee-custom)

;;;; JSLint with Flymake
;; http://emacswiki.org/emacs/FlymakeJavaScript

(when (load "flymake" t)
  (defun flymake-jslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "jslint" (list local-file))))

  (setq flymake-err-line-patterns
        (cons '("^  [[:digit:]]+ \\([[:digit:]]+\\),\\([[:digit:]]+\\): \\(.+\\)$"
                nil 1 2 3)
              flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-jslint-init)))

;;(add-hook 'js-mode-hook (lambda () (flymake-mode t)))
;;(add-hook 'js3-mode-hook (lambda () (flymake-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'renn-js)
