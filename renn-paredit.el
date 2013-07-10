(require 'paredit)
(require 'eldoc)

;;;; Extend Par Edit with ElDoc
;; from http://emacswiki.org/emacs/ParEdit

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return. ")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
       open and indent an empty line between the cursor and the text.  Move the
         cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode t)
            (turn-on-eldoc-mode)
            (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round)
            (local-set-key (kbd "RET") 'electrify-return-if-match)
            (eldoc-add-command 'electrify-return-if-match)
            ))
