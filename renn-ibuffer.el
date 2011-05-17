(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Org" (mode . org-mode))
               ("Python" (mode . python-mode))
               ("Special" (name . "*"))
               ("IRC" (mode . erc-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'renn-ibuffer)