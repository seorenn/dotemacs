;;;; Configurations for Emacs Lisp Development

;; Byte Compile on Save ELisp

(add-hook 'after-save-hook
          (lambda ()
            (if (eq major-mode 'emacs-lisp-mode)
                (save-excursion (byte-compile-file buffer-file-name)))))
