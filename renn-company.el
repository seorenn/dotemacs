;; Configurations for company-mode

(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

;; (defun indent-or-complete ()
;;   (interactive)
;;   (if (looking-at "\\_>")
;;       (company-complete-common)
;;     (indent-according-to-mode)))

;; (global-set-key "\t" 'indent-or-complete)
