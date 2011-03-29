(require 'erc)

;; put these info to renn-private.el
;; (setq my-irc-server "irc.hanirc.org")
;; (setq my-irc-port 6667)
;; (setq my-irc-nick "seorenn")
(defun my-irc ()
  "Connect IRC"
  (erc :server my-irc-server
       :port my-irc-port
       :nick my_irc_nick))