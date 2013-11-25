(defun iterm2-set-title (title)
  "Set iTerm2 Tab Title"
  (interactive)
  (send-string-to-terminal "\033]0;")
  (send-string-to-terminal title)
  (send-string-to-terminal "\007"))

(defun iterm2-set-subtitle (subtitle)
  "Change iTerm2 Tab Title to 'Emacs - subtitle'"
  (interactive)
  (iterm2-set-title (format "Emacs - %s" subtitle)))

(defun sr-when-buffer-changed ()
  (interactive)
  (iterm2-set-subtitle (buffer-name)))

;(add-hook 'change-major-mode-hook 'sr-when-buffer-changed)
