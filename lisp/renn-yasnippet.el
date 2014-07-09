;;;; YASnippets

;; fix cl--set-getf bug on Emacs 24.2
;(defalias 'cl-set-getf 'cl--set-getf) ; not works! :-(

(require 'yasnippet)
(yas-load-directory "~/.emacs.d/snippets")
(yas-global-mode 1)

;; Yasnippet with Web Mode
(defun yas-web-mode-fix ()
  (if (string= major-mode "web-mode")
      (progn
        (web-mode-buffer-refresh)
        (indent-for-tab-command))))
(setq yas/after-exit-snippet-hook 'yas-web-mode-fix)

;;;; for nxhtml-mode
;(yas-define-snippets 'nxhtml-mode nil 'html-mode)

;;;; Yasnippet with popup
;; (require 'popup)

;; (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
;; (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
;; (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
;; (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
;; (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

;; (defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
;;   (when (featurep 'popup)
;;     (popup-menu*
;;      (mapcar
;;       (lambda (choice)
;;         (popup-make-item
;;          (or (and display-fn (funcall display-fn choice))
;;              choice)
;;          :value choice))
;;       choices)
;;      :prompt prompt
;;      ;; start isearch mode immediately
;;      :isearch t
;;      )))

;; (setq yas/prompt-functions '(yas/popup-isearch-prompt yas/no-prompt))
