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

(global-set-key (kbd "C-s") 'memory-and-search)
(global-set-key (kbd "C-r") 'memory-and-search-backward)
(global-set-key (kbd "C--") 'back-to-search-point)

(provide 'renn-search)