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

(provide 'renn-ibuffer)
