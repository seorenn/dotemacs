;; python-mode on launchpad
;; https://launchpad.net/python-mode
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Pymacs + Ropemode
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;; Rope
(setq ropemacs-enable-autoimport t)

;; find TAG automatically...
(load "~/.emacs.d/renn-explorefile")
(add-hook 'python-mode-hook 'explore-tag)

;; and some python-mode settings
(add-hook 'python-mode-hook
          (lambda ()
            (set-variable 'py-indent-offset 4)
            (set-variable 'indent-tabs-mode nil)
            (smart-operator-mode-on)))

;; flymake using pyflakes
(require 'flymake)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'python-mode-hook 'flymake-find-file-hook)
;;(add-hook 'find-file-hook 'flymake-find-file-hook)

;; iPython
(setq ansi-color-for-comint-mode t)
(setq py-python-command-args '("-colors" "NoColor"))
(require 'ipython)
