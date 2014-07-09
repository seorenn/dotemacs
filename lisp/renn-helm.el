(require 'helm-config)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)

(require 'helm-swoop)

;; Custom Helm Integration

(defun helm-lisp-headlines ()
  "Display headlines for the current Lisp file."
  (interactive)
  (helm-mode t)
  (helm :sources '(((name . "Lisp Headlines")
                    (volatile)
                    (headline "^[;(]")))))

(defun helm-objc-headlines ()
  "Display headlines for the current Objective-C file."
  (interactive)
  (helm-mode t)
  (helm :sources '(((name . "Objective-C Headlines")
                    (volatile)
                    (headline "^[-+@]\\|^#pragma mark")))))

(defun helm-python-headlines ()
  "Display headlines for the current Python file."
  (interactive)
  (helm-mode t)
  (helm :sources '(((name . "Python Headlines")
                    (volatile)
                    (headline "\\(\\<def\\>\\|\\<class\\>\\|__main__\\)")))))
