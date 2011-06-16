(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; multi-term
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(setq multi-term-program "/bin/bash")

;; [C-x t] command open new term or switch next if one or more terms running
(global-set-key (kbd "C-x t") 'multi-term-next)
(global-set-key (kbd "C-x T") 'multi-term)

(add-hook 'term-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))