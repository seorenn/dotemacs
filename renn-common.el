(require 'hideshow)
(setq hs-minor-mode-hook nil)
(dynamic-completion-mode t)
(when window-system
  ; hide scroll bar
  (scroll-bar-mode -1))

; hide splash screen
(setq inhibit-splash-screen t)

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
(setq track-eol nil)
(setq tab-width 4)
(setq isearch-allow-scroll t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq delete-auto-save-files t)

(setq history-length 1000)
(savehist-mode t)
(setq recentf-max-saved-items 1000)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq diff-switches '("-u" "-p" "-N"))

(setq-default indent-tabs-mode nil)
(setq-default case-fold-search t)

(setq eval-expression-print-length nil)

(auto-image-file-mode t)
(delete-selection-mode nil)
(which-function-mode nil)
(global-font-lock-mode t)
(transient-mark-mode t)

(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;(global-linum-mode t)

(line-number-mode 1)
(column-number-mode 1)
(global-visual-line-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

;; whitespace-mode
(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
(global-whitespace-mode t)

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

;; Mode Line
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(setq mode-line-format
      (list
       '(:eval (propertize "%b" 'face 'font-lock-keyword-face
                           'help-echo (buffer-file-name)))

       "("
       (propertize "%02i" 'face 'font-lock-type-face) ","
       (propertize "%02c" 'face 'font-lock-type-face)
       ")"

       "["
       (propertize "%p" 'face 'font-lock-constant-face)
       "/"
       (propertize "%I" 'face 'font-lock-constant-face)
       "]"

       "["
       '(:eval (propertize "%m" 'face 'font-lock-string-face
                           'help-echo buffer-file-coding-system))
       "]"

       "["
       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                           'face 'font-lock-preprocessor-face
                           'help-echo (concat "Buffer is in "
                                              (if overwrite-mode "overwrite" "insert") " mode")))
       '(:eval (when (buffer-modified-p)
                 (concat "," (propertize "Mod"
                                         'face 'font-lock-warning-face
                                         'help-echo "Buffer has been modified"))))
       '(:eval (when buffer-read-only
                 (concat "," (propertize "RO"
                                         'face 'font-lock-type-face
                                         'help-echo "Buffer is read-only"))))
       "]"

       '(:eval (propertize (format-time-string "%H:%M")
                           'help-echo
                           (concat (format-time-string "%c; ")
                                   (emacs-uptime "Uptime:%hh"))))

       " --"
       "%-"))
