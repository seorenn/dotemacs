;; Seorenn Private Settings Templates for ERC

;; ERC

(require 'erc)

(setq erc-echo-notices-in-minibuffer-flag t)
(setq erc-default-coding-system 'utf-8)

(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.IRCSERVER.ADDRESS" "#CHANNEL"))
      )

(erc-track-mode t)
;; (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477"))
;; (setq erc-hide-list '("JOIN" "PART" "QUITE" "NICK"))

(defun escape-double-quote (str)
  "Escape Double Quotation Mark"
  (interactive)
  (replace-regexp-in-string "\"" "\\\\\"" str))

;; OS X Notification Function
;; You can install with Homebrew like this:
;;     $ brew install terminal-notifier
(defun notify-from-osx (title msg)
  "Call OS X Notification using terminal-notifier"
  (interactive)
  (let (bin cmd)
    (setq bin (executable-find "terminal-notifier"))
    (when bin
      (setq cmd (format "%s -title \"%s\" -message \"%s\" -sound default"
                        bin
                        (escape-double-quote title)
                        (escape-double-quote msg)))
      (message cmd)
      (shell-command cmd))))

(defun runerc ()
  "Connect to ERC"
  (interactive)
  (if (get-buffer "SERVER.ADDRESS") ;; ERC already activate?
      (erc-track-switch-buffer 1)
    (progn
      (erc :server "SERVER.ADDRESS" :nick "NICKNAME"))))

(defun renn-erc-hook (match-type nik message)
  "Notify Mention from IRC"
  (unless (posix-string-match "^\\** *Users on #" message)
    (notify-from-osx nik message)
    )
  )

(add-hook 'erc-text-matched-hook 'renn-erc-hook)
