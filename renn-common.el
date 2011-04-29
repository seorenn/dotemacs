(require 'hideshow)
(setq hs-minor-mode-hook nil)
(dynamic-completion-mode t)

(setq next-line-add-newlines nil)
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq hscroll-step 1)
(setq scroll-conservatively 10000)
(setq sentence-end-double-space nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq make-backup-files nil)
(setq next-line-add-newlines nil)
(setq track-eol nil)
(setq default-tab-width 4)
(setq isearch-allow-scroll t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq-default case-fold-search t)

(delete-selection-mode nil)
(which-function-mode t)
(global-font-lock-mode t)
(transient-mark-mode t)
(show-paren-mode t)
(line-number-mode 1)
(column-number-mode 1)
(global-visual-line-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

;; auto reload opened file on buffer when file edited by outside of emacs...
(global-auto-revert-mode t)

(add-to-list 'auto-mode-alist '("\\.outline'" . outline-mode))
(add-hook 'outline-mode-hook 'hide-body)

(setq ff-other-file-alist
      '(("\\.mm?$" (".h"))
        ("\\.cc$"  (".hh" ".h"))
        ("\\.hh$"  (".cc" ".C"))
        
        ("\\.c$"   (".h"))
        ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))
        
        ("\\.C$"   (".H"  ".hh" ".h"))
        ("\\.H$"   (".C"  ".CC"))

        ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
        ("\\.HH$"  (".CC"))

        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hpp" ".hh" ".h"))

        ("\\.hpp$" (".cpp" ".c"))))

(add-hook 'objc-mode-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)))
