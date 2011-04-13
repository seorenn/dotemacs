(add-to-list 'align-rules-list
             '(obj-c-colons
               (regexp . "^\\(\\s-*[^:]+\\):")
               (justify . t)
               (repeat . t)
               (modes obj-c-mode)))