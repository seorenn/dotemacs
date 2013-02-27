;;; egg-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (egg-minor-mode-find-file-hook egg-minor-mode)
;;;;;;  "egg" "egg.el" (20781 26617 0 0))
;;; Generated autoloads from egg.el

(autoload 'egg-minor-mode "egg" "\
Turn-on egg-minor-mode which would enable key bindings for
egg in current buffer.\\<egg-minor-mode-map>
\\[egg-start-new-branch] start a new branch from the current HEAD.
\\[egg-status] shows the repo's current status
\\[egg-commit-log-edit] start editing the commit message for the current staged changes.
\\[egg-file-stage-current-file] stage new changes of the current file
\\[egg-log] shows repo's history
\\[egg-file-checkout-other-version] checkout another version of the current file
\\[egg-file-cancel-modifications] delete unstaged modifications in the current file
\\[egg-next-action] perform the next logical action
\\[egg-file-diff] compare file with index or other commits
\\[egg-file-version-other-window] show other version of the current file.

\\{egg-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'egg-minor-mode-find-file-hook "egg" "\


\(fn)" nil nil)

(add-hook 'find-file-hook 'egg-git-dir)

(add-hook 'find-file-hook 'egg-minor-mode-find-file-hook)

;;;***

;;;### (autoloads (egg-grep egg-grep-mode egg-grep-process-setup)
;;;;;;  "egg-grep" "egg-grep.el" (20781 26617 0 0))
;;; Generated autoloads from egg-grep.el

(autoload 'egg-grep-process-setup "egg-grep" "\
Setup compilation variables and buffer for `egg-grep'.
Set up `compilation-exit-message-function' and run `egg-grep-setup-hook'.

\(fn)" nil nil)

(autoload 'egg-grep-mode "egg-grep" "\
Sets `compilation-last-buffer' and `compilation-window-height'.

\(fn)" nil nil)

(autoload 'egg-grep "egg-grep" "\


\(fn LEVEL)" t nil)

;;;***

;;;### (autoloads nil nil ("egg-pkg.el") (20781 26618 20844 0))

;;;***

(provide 'egg-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; egg-autoloads.el ends here
