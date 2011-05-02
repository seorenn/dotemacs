(add-to-list 'load-path "~/.emacs.d/vendor/twittering-mode")
(require 'twittering-mode)

(setq twittering-use-master-password t)
(setq twittering-icon-mode t)
(setq twittering-timer-interval 60)
(setq twittering-use-native-retweet t)
(setq twittering-scroll-mode t)
(setq twittering-fill-column 60)
(setq twittering-number-of-tweets-on-retrieval 100)

(setq twittering-status-format "%i %s %R\n%FILL[  ]{%T\n}          %@%r\n ")
(setq twittering-retweet-format "RT @%s %t")

(add-hook 'twittering-mode-hook
          (lambda ()
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key twittering-mode-map
                        (read-kbd-macro key) func)))
                  '(("1" . twittering-home-timeline)
                    ("2" . twittering-replies-timeline)
                    ("3" . twittering-direct-messages-timeline)
                    ("t" . twittering-retweet)
                    ))))

