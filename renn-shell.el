(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'tramp)
(custom-set-variables
 '(tramp-default-method "ssh")
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 ;; '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-buffer-maximum-size 100000)   ; max length of the buffer in lines
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
 '(protect-buffer-bury-p nil))

(setenv "PAGER" "cat")

;; eshell

(defun m-eshell-hook ()
  (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line))

(add-hook 'eshell-mode-hook 'm-eshell-hook)

;; multi-term
;;(autoload 'multi-term "multi-term" nil t)
;;(autoload 'multi-term-next "multi-term" nil t)

;;(setq multi-term-program "/bin/bash")

;; [C-x t] command open new term or switch next if one or more terms running
;;(global-set-key (kbd "C-x t") 'multi-term-next)
(global-set-key (kbd "C-x t") 'term)
;;(global-set-key (kbd "C-x T") 'multi-term)

(add-hook 'term-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))
