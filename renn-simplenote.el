(require 'simplenote)
(simplenote-setup)

;; Auto Sync Options

(defadvice save-buffer (before my-save-buffer activate)
  (when (equal (buffer-name) "*Simplenote*")
    (simplenote-sync-notes)
    (simplenote-browser-refresh)))

(add-hook 'before-save-hook
          (lambda ()
            (when (equal (buffer-name) "*Simplenote*")
              (simplenote-sync-notes)
              (simplenote-browser-refresh))))