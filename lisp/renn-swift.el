;; you can get this by using command:
;;   xcrun --show-sdk-path
(setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk")

(require 'swift-mode)

(add-hook 'swift-mode-hook
          (lambda ()
            (set-variable 'swift-indent-offset 2)
            (set-variable 'indent-tabs-mode nil)))
