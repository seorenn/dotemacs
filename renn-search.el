;;;; search current word
;; http://emacswiki.org/emacs/SearchAtPoint
;;
;; Move to beginning of word before yanking word in isearch-mode.
;; Make C-s C-w and C-r C-w act like Vim's g* and g#, keeping Emacs'
;; C-s C-w [C-w] [C-w]... behaviour.

(require 'thingatpt)

(defun my-isearch-yank-word-or-char-from-beginning ()
  "Move to beginning of word before yanking word in isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'word))
  (isearch-yank-word-or-char)
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition 'my-isearch-yank-word-or-char-from-beginning
                             'isearch-yank-word-or-char
                             isearch-mode-map))

(add-hook 'isearch-mode-hook
 (lambda ()
   "Activate my customized Isearch word yank command."
   (substitute-key-definition 'isearch-yank-word-or-char
                              'my-isearch-yank-word-or-char-from-beginning
                              isearch-mode-map)))


;;;; Search current curror word
;; http://www.emacswiki.org/emacs/SearchAtPoint
;; (extension) http://nschum.de/src/emacs/highlight-symbol/

(load-library "highlight-symbol")

(defun hl-symbol-and-jump ()
  (interactive)
  (let ((symbol (highlight-symbol-get-symbol)))
    (unless symbol (error "No symbol at point"))
    (unless hi-lock-mode (hi-lock-mode 1))
    (if (member symbol highlight-symbol-list)
        (highlight-symbol-next)
      (highlight-symbol-at-point)
      (highlight-symbol-next))))
(defun hl-symbol-cleanup ()
  (interactive)
  (mapc 'hi-lock-unface-buffer highlight-symbol-list)
  (setq highlight-symbol-list ()))

;;;; memory-and-search
;; memory current position and back to position

(defun memory-and-search ()
  (interactive)
  (when buffer-file-name
    (bookmark-set "search-point"))
  (isearch-forward))

(defun memory-and-search-backward ()
  (interactive)
  (when buffer-file-name
    (bookmark-set "search-point"))
  (isearch-backward))

(defun back-to-search-point ()
  (interactive)
  (bookmark-jump "search-point"))

(provide 'renn-search)
