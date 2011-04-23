(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; multi-term
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(setq multi-term-program "/bin/bash")

;; turn-off autopair on term-mode
;; (add-hook 'term-mode-hook
;;           '(lambda () (setq autopair-dont-activate t)))

;; [C-c t] command open new term or switch next if one or more terms running
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)