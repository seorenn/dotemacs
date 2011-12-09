;; renn-devel.el
;; This files set the Environment for Software Develomentation to Emacs

;; Auto Indentation for Development
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'lisp-mode-hook 'set-newline-and-indent)
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'c-mode-hook 'set-newline-and-indent)
(add-hook 'c++-mode-hook 'set-newline-and-indent)
(add-hook 'html-mode-hook 'set-newline-and-indent)

;; flymake mode
(require 'flymake)
(require 'flymake-cursor)

;; (defun my-flymake-show-error ()
;;   "Display flymake message from current line."
;;   (interactive)
;;   (flymake-display-err-menu-for-current-line))
;; (global-set-key [?\C-`] 'my-flymake-show-error)

;; icicles
;;(require 'icicles)
;;(icicle-mode 1)

;; dot-mode.el
;; "C-." dot-mode-excute
;; "C-M-." dot-mode-override
;; "C-c-." dot-mode-copy-to-last-kbd-macro
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

;; ----- C

(setq-default c-basic-offset 4
              indent-tabs-mode nil)

;; ----- Objective-C

(setq auto-mode-alist (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mm$" . objc-mode) auto-mode-alist))

;; Refered from:
;; https://github.com/jmjeong/jmjeong-emacs/blob/master/jmjeong-objc.el
(defun my-header-file-mode-hook ()
  (if (string-equal (file-name-extension buffer-file-name) "h")
	  (let ((filebase (file-name-sans-extension buffer-file-name)))
		(cond
		 ((file-exists-p (concat filebase ".c"))
		  (c-mode))
		 ((file-exists-p (concat filebase ".cpp"))
		  (c++-mode))
		 ((file-exists-p (concat filebase ".cc"))
		  (c++-mode))
		 ((file-exists-p (concat filebase ".m"))
		  (objc-mode))
		 ((file-exists-p (concat filebase ".mm"))
		  (objc-mode))
		 (t
		  (objc-mode))))))

(add-hook 'find-file-hook 'my-header-file-mode-hook)