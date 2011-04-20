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
;; this environments refered to http://sakito.jp/emacs/emacsobjectivec.html

;; bind objective-c mode to extensions .h, .m, .mm
;; note) objective-c has C syntax by super-set. So, it's compatible with C and C++
;; magic-mode-alist was turn-on mode when found some strings in buffers
;; (.m file is not objective-c only extensions)
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

