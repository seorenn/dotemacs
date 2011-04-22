;; renn-devel.el
;; This files set the Environment for Software Develomentation to Emacs

;; dot-mode.el
;; "C-." dot-mode-excute
;; "C-M-." dot-mode-override
;; "C-c-." dot-mode-copy-to-last-kbd-macro
(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)

;; ----- C

(setq-default c-basic-offset 4
              indent-tabs-mode nil)

;; ----- Python

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