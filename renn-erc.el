(require 'erc)

;; put these info to renn-private.el
;; (setq erc-my-irc-server "irc.hanirc.org")
;; (setq erc-my-irc-port 6667)
;; (setq erc-my-irc-nick "seorenn")

;; reference doc:
;; http://mwolson.org/static/doc/erc.html

(defun erc-my-irc ()
  "Connect IRC"
  (interactive)
  (erc :server erc-my-irc-server
       :port erc-my-irc-port
       :nick erc-my-irc-nick))