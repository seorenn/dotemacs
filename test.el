(require 'package)

(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(package-initialize)

(require 'clojure-mode)

(require 'nrepl)
