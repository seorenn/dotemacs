;;; egg-base.el --- Emacs Got Git - Emacs interface to Git

;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2011-12 byplayer
;;
;; Author: Bogolisk <bogolisk@gmail.com>
;; Created: 19 Aug 2008
;; Version: 1.0.2
;; Keywords: git, version control, release management
;;
;; Special Thanks to
;;   Antoine Levitt, Bogolisk,
;;   Christian Köstlin
;;   Max Mikhanosha
;;   Aleksandar Simic
;;
;; Egg is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Egg is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'egg-custom)
(require 'egg-base)

(defvar egg--internal-index-file nil)

(defsubst egg--git (buffer &rest args)
  "run GIT with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (= (apply 'call-process egg-git-command nil buffer nil args) 0))

(defsubst egg--git-args (buffer args)
  "run GIT with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (= (apply 'call-process egg-git-command nil buffer nil args) 0))

(defsubst egg--git-region (start end &rest args)
  "run GIT with ARGS and insert output into current buffer at point.
return the t if the exit-code was 0. The text between START and END
is used as input to GIT."
  (= (apply 'call-process-region start end egg-git-command t t nil args) 0))

(defsubst egg--git-region-args (start end args)
  "run GIT with ARGS and insert output into current buffer at point.
return the t if the exit-code was 0. The text between START and END
is used as input to GIT."
  (= (apply 'call-process-region start end egg-git-command t t nil args) 0))

(defsubst egg--git-check-index ()
  (if egg--internal-index-file
      (setenv-internal (copy-sequence process-environment)
		       "GIT_INDEX_FILE" egg--internal-index-file nil)
    process-environment))

(defmacro with-egg-index (&rest body)
  (declare (indent 0) (debug t))
  `(let ((process-environment (egg--git-check-index)))
     ,@body))


(defsubst egg-git-ok (buffer &rest args)
  "run GIT with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (with-egg-index (egg--git-args buffer args)))

(defsubst egg-git-ok-args (buffer args)
  "run GIT with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (with-egg-index (egg--git-args buffer args)))

(defsubst egg-git-region-ok (start end &rest args)
  "run GIT with ARGS and insert output into current buffer at point.
return the t if the exit-code was 0. The text between START and END
is used as input to GIT."
  (with-egg-index (egg--git-region-args start end args)))

(defsubst egg-git-region-ok-args (start end args)
  "run GIT with ARGS and insert output into current buffer at point.
return the t if the exit-code was 0. The text between START and END
is used as input to GIT."
  (with-egg-index (egg--git-region-args start end args)))

(defsubst egg-commit-contents (rev)
  "Retrieve the raw-contents of the commit REV."
  (with-temp-buffer
    (when (egg--git t "cat-file" "commit" rev)
      (buffer-string))))

(defsubst egg-commit-message (rev)
  "Retrieve the commit message of REV."
  (save-match-data
    (with-temp-buffer
      (when (egg--git t "cat-file" "commit" rev)
	(goto-char (point-min))
	(re-search-forward "^\n")
	(buffer-substring-no-properties (match-end 0) (point-max))))))

(defsubst egg-pick-from-commit-message (rev regex &optional index)
  "Retrieve the commit message of REV."
  (save-match-data
    (with-temp-buffer
      (when (egg--git t "cat-file" "commit" rev)
	(goto-char (point-min))
	(re-search-forward "^\n")
	(when (re-search-forward regex nil t)
	  (match-string-no-properties (or index 0)))))))

(defun egg-commit-subject (rev)
  "Retrieve the commit subject of REV."
  (save-match-data
    (with-temp-buffer
      (when (egg--git t "cat-file" "commit" rev)
	(goto-char (point-min))
	(re-search-forward "^\n")
	(buffer-substring-no-properties (match-end 0)
					(if (or (re-search-forward "\n\n" nil t)
						(re-search-forward "\n" nil t))
					    (match-beginning 0)
					  (point-max)))))))

(defsubst egg-cmd-to-string-1 (program args)
  "Execute PROGRAM and return its output as a string.
ARGS is a list of arguments to pass to PROGRAM."
  (with-temp-buffer
    (when (= (apply 'call-process program nil t nil args) 0)
      (buffer-substring-no-properties
       (point-min) (if (> (point-max) (point-min))
		       (1- (point-max)) (point-max))))))

(defsubst egg-cmd-to-string (program &rest args)
  "Execute PROGRAM and return its output as a string.
ARGS is a list of arguments to pass to PROGRAM."
  (egg-cmd-to-string-1 program args))

(defsubst egg-git-to-string-args (args)
  "run GIT wih ARGS and return the output as a string."
  (with-temp-buffer
    (when (egg-git-ok-args t args)
      (buffer-substring-no-properties
       (point-min) (if (> (point-max) (point-min))
		       (1- (point-max))
		     (point-max))))))

(defsubst egg--git-to-string-args (args)
  "run GIT wih ARGS and return the output as a string."
  (with-temp-buffer
    (when (egg--git-args t args)
      (buffer-substring-no-properties
       (point-min) (if (> (point-max) (point-min))
		       (1- (point-max))
		     (point-max))))))

(defsubst egg-git-to-string (&rest args)
  "run GIT wih ARGS and return the output as a string."
  (egg-git-to-string-args args))

(defsubst egg--git-to-string (&rest args)
  "run GIT wih ARGS and return the output as a string."
  (egg--git-to-string-args args))

(defun egg-git-show-file-args (buffer file rev args)
  (let* ((mode (assoc-default file auto-mode-alist 'string-match))
	 (extras (and mode (assoc-default mode egg-git-diff-file-options-alist 'eq))))
    (egg--git-args buffer (append (list "--no-pager" "show")
				  extras
				  args
				  (list rev "--" file)))))

(defsubst egg-git-show-file (buffer file rev &rest args)
  (egg-git-show-file-args buffer file rev args))

(defsubst egg-wdir-clean () (egg-git-ok nil "diff" "--quiet"))
(defsubst egg-file-updated (file)
  (egg-git-ok nil "diff" "--quiet" "--" file))
(defsubst egg-file-committed (file)
  (egg--git nil "diff" "--quiet" "HEAD" "--" file))
(defsubst egg-file-index-empty (file)
  (egg-git-ok nil "diff" "--quiet" "--cached" "--" file))
(defsubst egg-index-empty () (egg-git-ok nil "diff" "--cached" "--quiet"))

(defsubst egg-has-stashed-wip ()
  (egg--git nil "rev-parse" "--verify" "-q" "stash@{0}"))

(defsubst egg-git-to-lines (&rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (save-match-data
    (split-string (or (egg-git-to-string-args args) "") "[\n]+" t)))

(defsubst egg--git-to-lines (&rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (save-match-data
    (split-string (or (egg--git-to-string-args args) "") "[\n]+" t)))

(defsubst egg-git-to-string-list (&rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (save-match-data
    (split-string (or (egg-git-to-string-args args) "") "[\n\t ]+" t)))

(defsubst egg--git-to-string-list (&rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (save-match-data
    (split-string (or (egg--git-to-string-args args) "") "[\n\t ]+" t)))

(defun egg-git-lines-matching (re idx &rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (with-temp-buffer
    (when (egg-git-ok-args t args)
      (let (lines)
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (setq lines (cons (match-string-no-properties idx) lines)))
          lines)))))

(defun egg-git-lines-matching-stdin (stdin re idx &rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (with-temp-buffer
    (let (lines pos)
      (insert stdin)
      (setq pos (point-max))
      (when (egg-git-region-ok-args (point-min) (point-max) args)
        (save-match-data
          (goto-char pos)
          (while (re-search-forward re nil t)
            (setq lines (cons (match-string-no-properties idx) lines)))
          lines)))))

(defun egg-git-lines-matching-multi (re indices &rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (with-temp-buffer
    (when (egg-git-ok-args t args)
      (let (lines matches)
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (setq matches nil)
            (dolist (idx indices)
              (when (match-beginning idx)
                (setq matches
                      (cons (cons idx (match-string-no-properties idx))
                            matches))))
            (setq lines (cons matches lines)))
          lines)))))

(defsubst egg-git-range-length (young-commitish old-commitish)
  ;; add one for the base commit which was excluded by the ... notation
  (1+ (length (egg-git-to-lines "rev-list" (concat old-commitish "..." young-commitish)))))

(defsubst egg-file-git-name (file)
  "return the repo-relative name of FILE."
  (car (egg--git-to-lines "ls-files" "--full-name" "--" file)))

(defsubst egg-buf-git-name (&optional buf)
  "return the repo-relative name of the file visited by BUF.
if BUF was nil then use current-buffer"
  (egg-file-git-name (file-truename (buffer-file-name buf))))

(defsubst egg-files-git-name (files)
  "return the repo-relative name for each file in the list of files FILES."
  (delete-dups
   (apply 'egg--git-to-lines "ls-files" "--full-name" "--" files)))

(defsubst egg-unmerged-files ()
  "return a list of repo-relative names for each unmerged files."
  (save-match-data
    (delete-dups
     (mapcar 'car
             (mapcar 'last
                     (mapcar
                      'split-string
                      (egg-git-to-lines "ls-files" "--full-name" "-u")))))))

(defun egg--get-status-code ()
  (let ((lines (egg-git-to-lines "status" "--porcelain" "--untracked-files=no"))
	alist code index dir status)
    (dolist (line lines)
      (setq code (substring line 0 2))
      (setq index (aref code 0)
	    dir (aref code 1))
      (setq status nil)

      (cond ((and (= dir ? ) (memq index '(?M ?A ?R ?C))) 
	     (add-to-list 'status :wdir-index))
	    ((and (= dir ?M) (memq index '(?M ?A ?R ?C ? ))) 
	     (add-to-list 'status :wdir-modified))
	    ((= dir ?D)
	     (if (memq index '(?M ?A ?R ?C ? ))
		 (add-to-list 'status :wdir-deleted)
	       (add-to-list 'status :unmerged)
	       (cond ((= index ?D) (add-to-list 'status :both-deleted))
		     ((= index ?U) (add-to-list 'status :they-deleted)))))
	    ((= dir ?U)
	     (add-to-list 'status :unmerged)
	     (cond ((= index ?A) (add-to-list 'status :we-added))
		   ((= index ?D) (add-to-list 'status :we-deleted))
		   ((= index ?U) (add-to-list 'status :both-modified))))
	    ((= dir ?A)
	     (add-to-list 'status :unmerged)
	     (cond ((= index ?U) (add-to-list 'status :they-added))
		   ((= index ?A) (add-to-list 'status :both-added)))))

      (cond ((= index ? ) (add-to-list 'status :index-head))
	    ((and (= index ?M) (memq dir '(?M ?D ? ))) (add-to-list 'status :index-modified))
	    ((and (= index ?A) (memq dir '(?M ?D ? ))) (add-to-list 'status :index-added))
	    ((and (= index ?D) (memq dir '(?M ? ))) (add-to-list 'status :index-deleted))
	    ((and (= index ?R) (memq dir '(?M ?D ? ))) (add-to-list 'status :index-moved))
	    ((and (= index ?C) (memq dir '(?M ?D ? ))) (add-to-list 'status :index-copied)))
      
      (add-to-list 'alist (cons (substring line 3) status)))
    alist))

(defsubst egg-local-branches ()
  "Get a list of local branches. E.g. (\"master\", \"wip1\")."
  (egg--git-to-lines "rev-parse" "--symbolic" "--branches"))

(defsubst egg-local-refs ()
  "Get a list of local refs. E.g. (\"master\", \"wip1\")."
  (egg--git-to-lines "rev-parse" "--symbolic" "--branches" "--tags"))

(defun egg-remote-branches (&optional raw)
  "Get a list of remote branches. E.g. (\"origin/master\", \"joe/fork1\")."
  (let ((lst (egg--git-to-lines "rev-parse" "--symbolic" "--remotes")))
    (if raw lst
      (mapcar (lambda (full-name)
                (let ((tmp (save-match-data (split-string full-name "/"))))
                  (cons (cadr tmp) (car tmp))))
              lst))))

(defun egg-upstream (branch)
  (and (egg--git nil "config" (concat "branch." branch ".merge"))
       (let ((upstream (egg-git-to-string "name-rev" "--name-only" 
					  (concat branch "@{upstream}"))))
	 (if (and (> (length upstream) 8)
		  (string-equal (substring upstream 0 8) "remotes/"))
	     (substring upstream 8)
	   upstream))))






(defsubst egg-is-in-git ()
  "is the default-directory in a git repo."
  (egg--git nil "rev-parse" "--git-dir"))

(defsubst egg-is-dir-in-git (dir)
  "is DIR in a git repo."
  (let ((default-directory dir)) (egg-is-in-git)))

(defsubst egg-name-rev (rev)
  "get the symbolic name of REV."
  (egg--git-to-string "name-rev" "--always" "--name-only" rev))

(defun egg-pretty-short-rev (rev)
  (let ((rev (egg-name-rev rev))
	(short-sha (and rev (substring (egg-git-to-string "rev-parse" rev) 0 8))))
    (save-match-data
      (when (string-match "\\`\\(remotes\\|tags\\)/" rev)
	(setq rev (substring rev (match-end 0)))))
    (if (> (length rev) 24)
	short-sha
      rev)))

(defsubst egg-git-canon-name (rev file)
  (when (and rev file)
    (concat rev ":" (egg-file-git-name file))))

(defsubst egg-describe-rev (rev)
  "get the long symbolic name of REV."
  (egg--git-to-string "describe" "--always" "--tags" rev))

(defsubst egg-sha1 (rev)
  "get the SHA1 of REV."
  (egg--git-to-string "rev-parse" (concat rev "~0")))

(defun egg-completing-read-sha1 (from prompt &optional default max-back)
  (let* ((max-count (or max-back 100))
	 (sha1-list (egg--git-to-lines "rev-list" "--topo-order"
				      (format "--max-count=%d" max-count)
				      "--abbrev-commit" 
				     from))
	 (sha1-hist (copy-sequence sha1-list)))
    (egg-sha1 (completing-read prompt sha1-list nil nil default 'sha1-hist))))

(defun egg-read-git-dir ()
  "call GIT to read the git directory of default-directory."
  (let* ((dotgit-parent (locate-dominating-file default-directory ".git"))
	 (dotgit (and dotgit-parent (concat dotgit-parent "/.git")))
         (dir (or (and dotgit (file-directory-p dotgit) dotgit)
		  (egg--git-to-string "rev-parse" "--git-dir")))
	 (work-tree dotgit-parent))
    (when (stringp dir)
      (setq dir (expand-file-name dir))
      (when (stringp work-tree) 
	(setq work-tree (expand-file-name work-tree))
	(put-text-property 0 (length dir) :work-tree work-tree dir))
      dir)))

(defvar egg-git-dir nil)
(defun egg-git-dir (&optional error-if-not-git)
  "return the (pre-read) git-dir of default-directory"
  (if (and (local-variable-p 'egg-git-dir) egg-git-dir)
      egg-git-dir
    (set (make-local-variable 'egg-git-dir)
         (or (egg-read-git-dir)
             (and error-if-not-git
                  (or (kill-local-variable 'egg-git-dir) t)
                  (error "Not in a git repository: %s" default-directory))))
    ;; first time, no status yet.
    ;; this directory's specific var will be updated by
    ;; egg-set-mode-info
    (set (intern (concat "egg-" egg-git-dir "-HEAD")) " Egg")
    egg-git-dir))

(defsubst egg-work-tree-dir (&optional git-dir fall-back)
  (unless git-dir (setq git-dir (egg-git-dir)))
  (if git-dir 
      (or (get-text-property 0 :work-tree git-dir)
	  (file-name-directory git-dir))
    fall-back))

(defun egg-file-get-index-buffer ()
  (let* ((git-name (egg-file-git-name (buffer-file-name)))
	 (default-directory (egg-work-tree-dir)))
    (get-buffer (concat "*:0:" git-name "*"))))

(defun egg-index-get-file-visiting-buffer ()
  (let* ((default-directory (egg-work-tree-dir))
	 (file-name (buffer-local-value 'egg-git-name (current-buffer))))
    (find-buffer-visiting file-name)))

(defsubst egg-repo-name (&optional git-dir)
  (let* ((dir (or git-dir (egg-git-dir)))
	 (work-tree-dir (egg-work-tree-dir dir)))
    (when (stringp work-tree-dir)
      (file-name-nondirectory (directory-file-name work-tree-dir)))))

(defsubst egg-buf-git-dir (buffer)
  "return the (pre-read) git-dir of BUFFER."
  (with-current-buffer buffer
    (egg-git-dir)))

(defun egg-commit-parents (rev)
  (let ((default-directory (egg-work-tree-dir))
	parents)
    (with-temp-buffer
      (egg--git t "--no-pager" "cat-file" "-p" rev)
      (goto-char (point-min))
      (while (re-search-forward (rx line-start 
				    "parent " (group (= 40 hex-digit)) 
				    (0+ space)
				    line-end) nil t)
	(add-to-list 'parents (match-string-no-properties 1)))
      (setq parents (mapcar (lambda (long)
			      (substring-no-properties long 0 8))
			    (nreverse parents))))
    parents))

(defun egg-HEAD ()
  "return HEAD. Either a symbolic ref or a sha1."
  (let* ((git-dir (egg-git-dir)))
    (if git-dir
        (egg-pick-file-contents (concat git-dir "/HEAD")
                                "^ref: refs/heads/\\(.+\\)\\|^\\([0-9a-f]+\\)" 1 2))))

(defsubst egg-get-symbolic-HEAD (&optional file)
  ;; get the symbolic name of HEAD
  (setq file (or file (concat (egg-git-dir) "/HEAD")))
  (egg-pick-file-contents file
                          "^ref: refs/heads/\\(.+\\)"
                          1))

(defsubst egg-get-full-symbolic-HEAD (&optional file)
  ;; get the symbolic full name of HEAD
  (setq file (or file (concat (egg-git-dir) "/HEAD")))
  (egg-pick-file-contents file
                          "^ref: \\(refs/heads/.+\\)"
                          1))

(defsubst egg-get-current-sha1 ()
  (or (egg-git-to-string "rev-parse" "--verify" "-q" "HEAD")
      "0000000000000000000000000000000000000000"))

(defun egg-get-all-refs (prefix &optional exact)
  (if exact
      (egg-git-to-lines "for-each-ref" "--format=%(refname:short)" 
			(format "refs/heads/%s" prefix)
			(format "refs/tags/%s" prefix)
			(format "refs/remotes/%s" prefix))
    (egg-git-to-lines "for-each-ref" "--format=%(refname:short)" 
		      (format "refs/heads/%s*" prefix)
		      (format "refs/tags/%s*" prefix)
		      (format "refs/remotes/%s*/*" prefix)
		      (format "refs/remotes/%s*" prefix))))


(defun egg-get-local-refs (prefix)
  (egg-git-to-lines "for-each-ref" "--format=%(refname:short)" 
		    (format "refs/heads/%s*" prefix)
		    (format "refs/tags/%s*" prefix)))

(defsubst egg-git-rebase-dir (&optional git-dir)
  (concat (or git-dir (egg-git-dir)) "/" egg-git-rebase-subdir "/"))

(defsubst egg-rebase-author-info (rebase-dir)
  "Retrieve an alist of commit environment variables of the current
cherry in REBASE-DIR."
  (mapcar (lambda (lst)
            ;; chop the ' '
            (setcar (cdr lst) (substring (cadr lst) 1 -1))
            lst)
          (mapcar (lambda (line)
                    ;; name-value split
                    (save-match-data (split-string line "=" t)))
                  ;; grab the GIT_xxx=yyy
                  (egg-pick-file-records (concat rebase-dir "author-script")
                                         "^GIT_\\(.+\\)" "$"))))

(defsubst egg-interactive-rebase-in-progress ()
  "Is an interactive rebase in progress in the current repo?"
  (file-exists-p (concat (egg-git-dir) "/" egg-git-rebase-subdir
                         "/interactive") ))

(defun egg-rebase-in-progress ()
  (plist-get (egg-repo-state) :rebase-step))


(defsubst egg-get-rebase-apply-state (rebase-dir)
  "Build a plist of rebase info of REBASE-DIR.
this is for rebase -m variant."
  (let ((patch-files (directory-files rebase-dir nil "\\`[0-9]+\\'")))
    (list :rebase-dir rebase-dir
        :rebase-head (egg-pretty-short-rev 
		      (egg-file-as-string (concat rebase-dir "head-name")))
        :rebase-upstream
        (egg-pretty-short-rev (egg-file-as-string (concat rebase-dir "onto")))
        :rebase-step (string-to-number (car patch-files))
        :rebase-num (string-to-number (car (nreverse patch-files))))))

(defsubst egg-get-rebase-merge-state (rebase-dir)
  "Build a plist of rebase info of REBASE-DIR.
this is for rebase -m variant."
  (list :rebase-dir rebase-dir
        :rebase-head
        (egg-pretty-short-rev (egg-file-as-string (concat rebase-dir "head-name")))
        :rebase-upstream
        (egg-pretty-short-rev (egg-file-as-string (concat rebase-dir "onto_name")))
        :rebase-step			;; string-to-number?
        (egg-file-as-string (concat rebase-dir "msgnum"))
        :rebase-num			;; string-to-number?
        (egg-file-as-string (concat rebase-dir "end"))))

(defsubst egg-get-rebase-interactive-state (rebase-dir)
  "Build a plist of rebase info of REBASE-DIR.
this is for rebase -i variant."
  (list :rebase-dir rebase-dir
        :rebase-head
        (egg-pretty-short-rev (egg-file-as-string (concat rebase-dir "head-name")))
        :rebase-upstream
        (egg-pretty-short-rev (egg-file-as-string (concat rebase-dir "onto")))
        :rebase-num
        (length
         (egg-pick-file-records (concat rebase-dir "git-rebase-todo.backup")
                                "^[pesf]" "$"))
        :rebase-step
        (if (file-exists-p (concat rebase-dir "done"))
            (length (egg-pick-file-records (concat rebase-dir "done")
                                           "^[pesf]" "$"))
          0)
	:rebase-stopped
	(if (file-exists-p (concat rebase-dir "stopped-sha"))
	    (egg-pick-file-contents (concat rebase-dir "stopped-sha") "^[0-9a-f]+$"))
        :rebase-cherry
        (if (file-exists-p (concat rebase-dir "done"))
            (car (egg-pick-file-records
                  (concat rebase-dir "done")
                  "^[pesf]" "$")))))

(defsubst egg-set-mode-info (state)
  "Set the mode-line string for buffers visiting files in the current repo.
The string is built based on the current state STATE."
  (set (intern (concat "egg-" egg-git-dir "-HEAD"))
       (format " Git:%s" (cond ((plist-get state :rebase-dir)
                                "(rebasing)")
                               ((plist-get state :merge-heads)
                                "(merging)")
			       ((plist-get state :squash-head)
                                "(squashing)")
                               ((plist-get state :branch)
                                (plist-get state :branch))
                               (t "(detached)")))))

(defvar egg-internal-current-state nil)
(defun egg-get-repo-state (&optional extras)
  "Retrieve current repo's state as a plist.
The properties:
:gitdir :head :branch :sha1 :merge-heads :rebase-dir :rebase-head
:rebase-upstream :rebase-step :rebase-num :rebase-cherry

EXTRAS contains the extra properties to retrieve: :staged :unstaged

if EXTRAS contains :error-if-not-git then error-out if not a git repo.
"
  (let* ((git-dir (egg-git-dir (memq :error-if-not-git extras)))
         (head-file (concat git-dir "/HEAD"))
         (merge-file (concat git-dir "/MERGE_HEAD"))
	 (squash-file (concat git-dir "/SQUASH_MSG"))
         (branch (egg-get-symbolic-HEAD head-file))
         (branch-full-name (egg-get-full-symbolic-HEAD head-file))
         (sha1 (egg-get-current-sha1))
         (merge-heads
          (mapcar 'egg-pretty-short-rev
                  (if (file-readable-p merge-file)
                      (egg-pick-file-records merge-file "^" "$"))))
	 (squash-head (when (file-readable-p squash-file)
			(egg-pick-file-contents squash-file (rx line-start "commit " 
								(group (= 40 hex-digit))
								line-end)
						1)))
	 (rebase-apply (if (file-directory-p (concat git-dir "/rebase-apply"))
			   (concat git-dir "/rebase-apply/")))
         (rebase-dir
	  (or rebase-apply
	      (if (file-directory-p (concat git-dir "/" egg-git-rebase-subdir))
		  (concat git-dir "/" egg-git-rebase-subdir "/"))))
         (is-rebase-interactive 
	  (and (not rebase-apply)
	       (file-exists-p (concat rebase-dir "interactive"))))
         (rebase-state
          (if rebase-apply
	      (egg-get-rebase-apply-state rebase-dir)
	      (when rebase-dir
		(if is-rebase-interactive
		    (egg-get-rebase-interactive-state rebase-dir)
		  (egg-get-rebase-merge-state rebase-dir)))))
         (state (nconc (list :gitdir git-dir
                             :head branch-full-name
                             :branch branch
                             :sha1 sha1
			     :squash-head (and squash-head (egg-pretty-short-rev squash-head))
                             :merge-heads merge-heads)
                       rebase-state))
         files)
    (dolist (req extras)
      (cond ((eq req :unstaged)
             (setq files (egg-git-to-lines "diff" "--name-only"))
             (setq state (nconc (list :unstaged files) state))
             (when (and files (stringp (car files)))
               (setq state (nconc (list :unmerged (egg-unmerged-files))
                                  state))))
            ((eq req :staged)
             (setq state
                   (nconc (list :staged (egg-git-to-lines "diff" "--cached" "--name-only"))
                          state)))
	    
	    ((eq req :name)
             (setq state
                   (nconc (list :name (egg-git-to-string "config" "user.name")) state)))
	    ((eq req :email)
             (setq state
                   (nconc (list :email (egg-git-to-string "config" "user.email")) state)))))
    ;; update mode-line
    (egg-set-mode-info state)
    state))

(defun egg-repo-state (&rest args)
  "return the cached repo state or re-read it.
if ARGS contained :force then ignore the cached state."
  (if (or (null egg-internal-current-state) ;; not cached
	  (memq :force args)		    ;; forced
	  (memq nil ;; cached copy has no extra reqs
		(mapcar (lambda (req)
			  (memq req egg-internal-current-state))
			args)))
      (egg-get-repo-state args)
    egg-internal-current-state))

(defsubst egg-repo-clean (&optional state)
  "Whether the current repos is clean base on the current repo state.
use STATE as repo state if it was not nil. Otherwise re-read the repo state."
  (unless state
    (setq state (egg-repo-state :staged :unstaged)))
  (and
   (null (plist-get state :rebase-num))
   (null (plist-get state :merge-heads))
   (null (plist-get state :squash-head))
   (not (if (memq :unstaged state)
            (plist-get state :unstaged)
          (egg-wdir-clean)))
   (not (if (memq :staged state)
            (plist-get state :staged)
          (egg-index-empty)))))

(defsubst egg-is-merging (state)
  (or (plist-get state :merge-heads)
      (plist-get state :rebase-dir)
      (plist-get state :squash-head)))

(defun egg-wdir-dirty () (plist-get (egg-repo-state :unstaged) :unstaged))
(defun egg-staged-changes () (plist-get (egg-repo-state :staged) :staged))

(defsubst egg-current-branch (&optional state)
  "The current symbolic value of HEAD. i.e. name of a branch. if STATE
was not nil then use it as repo state instead of re-read from disc."
  (plist-get (or state (egg-repo-state)) :branch))

(defsubst egg-current-sha1 (&optional state)
  "The immutable sha1 of HEAD.  if STATE was not nil then use it
as repo state instead of re-read from disc."
  (plist-get (or state (egg-repo-state)) :sha1))

(defsubst egg-short-sha1 (&optional sha1)
  (egg-git-to-string "rev-parse" "--short" (or sha1 (egg-current-sha1))))

(defsubst egg-user-name (&optional state)
  "The configured user name."
  (plist-get (or state (egg-repo-state :name)) :name))

(defsubst egg-user-email (&optional state)
  "The configured email."
  (plist-get (or state (egg-repo-state :email)) :email))

(defsubst egg-head (&optional state)
  "a cons cell (branch . sha1) of HEAD.  if STATE was not nil then use it
as repo state instead of re-read from disc."
  (if (egg-git-dir)
      (let ((state (or state (egg-repo-state))))
        (cons (egg-current-sha1 state)
              (egg-current-branch state)))))

(defsubst egg-branch-or-HEAD () (or (egg-get-symbolic-HEAD) "HEAD"))


(defsubst egg-config-section-raw (type &optional name)
  (egg-pick-file-contents (concat (egg-git-dir) "/config")
                          (concat "^"
                                  (if name
                                      (format "\\[%s \"%s\"\\]" type name)
                                    (format "\\[%s\\]" type))
                                  "\n"
                                  "\\(\\(?:\t.+\n\\)+\\)")
                          1))

(defsubst egg-config-section (type &optional name)
  (save-match-data
    (mapcar
     (lambda (line)
       (split-string line "[ =]+" t))
     (split-string (or (egg-config-section-raw type name) "")
                   "[\t\n]+" t))))

(defun egg-config-get-all (called-interactively file type)
  (interactive "p\nfFilename: \nsType: ")
  (let (res)
    (setq res
	  (save-match-data
	    (mapcar (lambda (rec)
		      (let ((key (car rec))
			    (infos (cdr rec)))
			(cons (progn (string-match "\"\\(.+\\)\"" key)
				     (match-string-no-properties 1 key))
			      (mapcar (lambda (attr)
					(split-string attr "[ =]+" t))
				      infos))))
		    (mapcar (lambda (str)
			      (split-string str "[\t\n]+" t))
			    (egg-pick-file-records file
						   (concat "^\\[" type " \"")
						   "^\\[\\|\\'")))))
    (if called-interactively
	(message "%S" res))
    res))

(defsubst egg-config-get-all-branches ()
  (egg-config-get-all nil (concat (egg-git-dir) "/config") "branch"))

(defsubst egg-config-get-all-remotes ()
  (egg-config-get-all nil (concat (egg-git-dir) "/config") "remote"))

(defsubst egg-config-get-all-remote-names ()
  (nconc (mapcar 'car (egg-config-get-all-remotes))
	 (egg-get-all-remotes)))

(defsubst egg-config-get (type attr &optional name)
  (and (egg-git-dir)
       (cadr (assoc attr (egg-config-section type name)))))

(defun egg-tracking-target (branch &optional mode)
  (let ((remote (egg-config-get "branch" "remote" branch))
        (rbranch-full (egg-config-get "branch" "merge" branch))
	rbranch)
    (when (stringp rbranch-full)
      (setq rbranch (egg-rbranch-name rbranch-full))
      (cond ((null mode) (concat remote "/" rbranch))
            ((eq :name-only mode) rbranch)
	    ((eq :remote mode) (list rbranch-full remote))
            (t (list rbranch remote))))))

(defun egg-complete-get-all-refs (prefix &optional matches)
  (if matches
      (try-completion prefix matches)
    (egg-get-all-refs prefix)))

(defun egg-complete-get-local-refs (prefix &optional matches)
  (if matches
      (try-completion prefix matches)
    (egg-get-local-refs prefix)))

(defun egg-get-match-files-substring (sub &optional matches)
  (if matches
      (try-completion sub (mapcar #'file-name-nondirectory matches))
    (let ((default-directory (egg-work-tree-dir))
	  files name-matched-files full-match)
      (setq files (egg-git-to-lines "--no-pager" "ls-files" 
				    (concat sub "*")
				    (concat "*/" sub "*")))
      (dolist (file files)
	(if (string-equal file sub)
	    (setq full-match file))
	(if (string-equal (file-name-nondirectory file) sub)
	    (add-to-list 'name-matched-files file)))
      (or (and full-match (list full-match))
	  name-matched-files
	  files))))

(defun egg-do-completion (string &optional func all)
  "Do ref name completion"
  (let* ((matches (funcall func string))
	 (single (= (length matches) 1))
	 (perfect (and single (equal (car matches) string)))
	 prefix)

    (if all matches
      (when matches
	(setq prefix (funcall func string matches)))
      (cond ((null matches) nil)
	    (perfect t)
	    (single (car matches))
	    ((stringp prefix) prefix)
	    ((null prefix) nil)
	    (t string)))))

(defsubst egg-read-ref (prompt &optional default no-match-ok)
  (completing-read prompt #'egg-do-completion #'egg-complete-get-all-refs (not no-match-ok) default))

(defsubst egg-read-local-ref (prompt &optional default no-match-ok)
  (completing-read prompt #'egg-do-completion #'egg-complete-get-local-refs (not no-match-ok) default))

(defsubst egg-read-rev (prompt &optional default)
  "Query user for a revision using PROMPT. DEFAULT is the default value."
  (completing-read prompt 'egg-complete-rev nil nil default))

(defvar egg-add-remote-properties nil)
(defun egg-add-remote-properties (name remote &optional branch)
  (or (and (functionp egg-add-remote-properties)
	   (funcall egg-add-remote-properties name remote branch))
      (and (consp egg-add-remote-properties)
	   (let ((name name))
	     (dolist (func egg-add-remote-properties)
	       (setq name (funcall func name remote branch)))
	     name))
      name))

(defvar egg-get-remote-properties nil)
(defun egg-get-remote-properties (remote branch)
  (cond ((functionp egg-get-remote-properties)
	 (funcall egg-get-remote-properties remote branch))
	((consp egg-get-remote-properties)
	 (dolist-done (func egg-get-remote-properties props)
	   (setq props (funcall func remote branch))))
	(t nil)))

(defsubst egg-read-remote (prompt &optional default)
  "Query user for a remote using PROMPT. DEFAULT is the default value."
  (let ((remote (completing-read prompt (egg-config-get-all-remote-names) nil t default)))
    (egg-add-remote-properties remote remote)))

(defvar egg-get-all-remotes nil)
(defun egg-get-all-remotes ()
  (cond ((functionp egg-get-all-remotes)
	 (funcall egg-get-all-remotes))
	((consp egg-get-all-remotes)
	 (apply #'append (mapcar #'funcall egg-get-all-remotes)))
	(t nil)))

(defun egg-full-ref-decorated-alist (head-properties
                                     tag-properties
                                     atag-properties
                                     remote-ref-properties
                                     remote-site-properties
				     &optional head-properties-HEAD stash-properties)
  "Build an alist of (ref . :type) cells.
A ref string of a head will be decorated with HEAD-PROPERTIES.  A
ref string of a tag will be decorated with TAG-PROPERTIES or
ATAG-PROPERTIES.  A ref string of a remote will be formatted with
REMOTE-REF-PROPERTIES and REMOTE-SITE-PROPERTIES."
  (let* ((ref-re
	  (rx line-start
	      (one-or-more not-newline)
	      (group "refs/"
		     (or (seq (or (group "heads") 
				  (group "tags") 
				  (group "remotes"))
			      "/"
			      (group (optional (seq (group (one-or-more 
							    (not (any ?\n blank ?^ ?/))))
						    "/")) 
				     (one-or-more (not (any ?\n blank ?^ ?/)))))
			 (group "stash")))
	      (optional (group "^{}"))
	      line-end))
	 (refs-desc-list
         (egg-git-lines-matching-multi ref-re
          ;; 1: full-name
          ;; 2: head
          ;; 3: tag
          ;; 4: remote
          ;; 5: name
          ;; 6: remote-host
          ;; 7: stash
          ;; 8: is annotated tag
          '(1 2 3 4 5 6 7) "show-ref" "-d"))
	(symbolic-HEAD (egg-get-symbolic-HEAD))
        annotated-tags refs)
    ;; remove the annotated tags from the list
    (setq refs-desc-list
          (delq nil
                (mapcar (lambda (desc)
                          (if (not (assq 8 desc))
                              ;; not an annotated tag
                              desc
                            (setq annotated-tags
                                  (cons (cdr (assq 1 desc))
                                        annotated-tags))
                            nil))
                        refs-desc-list)))
    ;; decorate the ref alist
    (setq refs
	  (mapcar (lambda (desc)
		    (let ((full-name (cdr (assq 1 desc)))
			  (name (cdr (or (assq 5 desc) (assq 7 desc))))
			  (remote (cdr (assq 6 desc)))
			  short-name)
		      (cond ((assq 2 desc)
			     ;; head
			     (cons full-name
				   (apply 'propertize name
					  :full-name full-name
					  :ref (cons name :head)
					  (if (and head-properties-HEAD
						   (string-equal name symbolic-HEAD))
					      head-properties-HEAD
					    head-properties))))
			    ((assq 3 desc)
			     ;; tag
			     (cons full-name
				   (apply 'propertize name
					  :full-name full-name
					  :ref (cons name :tag)
					  (if (member full-name annotated-tags)
					      atag-properties
					    tag-properties))))
			    ((assq 4 desc)
			     ;; remote
			     (cons full-name
				   (egg-add-remote-properties 
				    (concat (apply 'propertize remote
						   :ref (cons name :remote)
						   remote-site-properties)
					    (apply 'propertize (substring name (length remote))
						   :full-name full-name
						   :ref (cons name :remote)
						   remote-ref-properties)) remote full-name)))
			    ((assq 7 desc)
			     ;; stash
			     (cons full-name
				   (apply 'propertize name 
					  :full-name full-name
					  :ref (cons name :stash)
					  stash-properties))))))
            refs-desc-list))
    (unless symbolic-HEAD
      (setq refs (cons (cons "HEAD" (propertize "HEAD" 'face 'egg-log-HEAD-name)) refs)))
    refs))

(defun egg-complete-rev (string &optional ignored all)
  "Do revision completion"
  (save-match-data
    (cond ((string-match "\\`:[0-3]*" string) ;; stages
           (funcall (if all 'all-completions 'try-completion)
                    string '(":0" ":1" ":2" ":3")))

          ;; rev^, rev~10 etc.
          ((string-match "[\\^~][\\^~0-9]*\\'" string)
           ;; check with rev-parse
           (if (egg--git nil "rev-parse" string)
               ;; rev-parse ok
               (if all
                   ;; fixme: how to do a full expansion?
                   (list string)
                 ;; match
                 string)))

          ;; normal rev name
          (t (let ((matches
                    ;; match all types of refs
                    (egg-git-to-lines "for-each-ref" "--format=%(refname)"
                                      (concat "refs/*/" string "*")
                                      (concat "refs/*/" string "*/*")))
                   prefix)
               ;; get the short name
               ;; with 1.6.x: for-each-ref" "--format=%(refname=short)
               (setq matches
                     (mapcar (lambda (long)
                               (string-match
                                "\\`refs/\\(?:heads\\|tags\\|remotes\\)/\\(.+\\)\\'"
                                long)
                               (match-string-no-properties 1 long))
                             matches))
               ;; do the completion
               (setq prefix
                     (funcall (if all 'all-completions 'try-completion)
                              string
                              (nconc (directory-files (egg-git-dir)
                                                      nil "HEAD")
                                     matches)))
               (cond (all prefix)
                     ((stringp prefix) prefix)
                     ((null prefix) nil)
                     (t string)))))))


(defmacro with-egg-debug-buffer (&rest body)
  "Evaluate BODY there like `progn' in the egg's debug buffer.
See also `with-temp-file' and `with-output-to-string'."
  (declare (indent 0) (debug t))
  (let ((egg-debug-buffer (make-symbol "egg-debug-buffer"))
	(egg-debug-dir (make-symbol "egg-debug-dir")))
    `(let ((,egg-debug-dir (egg-work-tree-dir))
	   (,egg-debug-buffer (get-buffer-create (concat "*egg-debug:" (egg-git-dir) "*"))))
       (with-current-buffer ,egg-debug-buffer
	 (setq default-directory ,egg-debug-dir)
         (unwind-protect
	     (progn ,@body)
           )))))

(defmacro with-egg-async-buffer (&rest body)
  "Evaluate BODY there like `progn' in the egg's async buffer.
See also `with-temp-file' and `with-output-to-string'."
  (declare (indent 0) (debug t))
  (let ((egg-async-buffer (make-symbol "egg-async-buffer"))
	(egg-async-dir (make-symbol "egg-async-dir")))
    `(let ((,egg-async-dir (egg-work-tree-dir))
	   (,egg-async-buffer (get-buffer-create (concat "*egg-async:" (egg-git-dir) "*"))))
       ;; FIXME: kill-buffer can change current-buffer in some odd cases.
       (with-current-buffer ,egg-async-buffer
	 (setq default-directory ,egg-async-dir)
         (unwind-protect
	     (progn ,@body)
           )))))

;; (cl-macroexpand '(with-egg-debug-buffer (do-something)))
;; (let* ((egg-debug-dir (egg-work-tree-dir))
;;        (egg-debug-buffer (get-buffer-create (concat "*egg-debug:" (egg-git-dir) "*"))))
;;   (with-current-buffer egg-debug-buffer 
;;     (setq default-directory egg-debug-dir)
;;     (unwind-protect 
;; 	(progn (do-something))
;;       (and (buffer-name egg-debug-buffer) 
;; 	   (kill-buffer egg-debug-buffer)))))

;;;========================================================
;;; Async Git process
;;;========================================================
(defvar egg-buffer-refresh-func nil)
(defvar egg-buffer-async-cmd-refresh-func nil)

(defun egg-run-buffers-update-hook (&optional newly-read-state)
  "Update all egg special buffers."
  (let ((egg-internal-current-state
         (or newly-read-state (egg-get-repo-state))))
    (run-hooks 'egg-buffers-refresh-hook)))


(defsubst egg-async-process ()
  (with-egg-async-buffer
   (let ((proc (get-buffer-process (current-buffer))))
     (if (and (processp proc) 		;; is a process
	      (not (eq (process-status proc) 'exit)) ;; not finised
	      (= (process-exit-status proc) 0))      ;; still running
	 proc))))

(defun egg-async-do (exit-code func-args args)
  "Run GIT asynchronously with ARGS.
if EXIT code is an exit-code from GIT other than zero but considered
success."
  (let ((inhibit-read-only inhibit-read-only)
        (accepted-msg (and (integerp exit-code)
                           (format "exited abnormally with code %d"
                                   exit-code)))
	(command egg-git-command)
	(proc-name "egg-git")
        proc tmp)

    (when (setq tmp (plist-get args :program))
      (setq command tmp)
      (plist-put args :program nil)
      (setq args (delq nil (remove :program args))))

    (when (setq tmp (plist-get args :process-name))
      (setq proc-name tmp)
      (plist-put args :process-name nil)
      (setq args (delq nil (remove :process-name args))))
    
    (with-egg-async-buffer
      (setq proc (get-buffer-process (current-buffer)))
      (when (and (processp proc)		       ;; is a process
		 (not (eq (process-status proc) 'exit)) ;; not finised
		 (= (process-exit-status proc) 0)) ;; still running
	(error "EGG: %s is already running!" (process-command proc)))
      (setq inhibit-read-only t)
      (erase-buffer)
      (widen)
      (goto-char (point-max))
      (insert "EGG-GIT-CMD:\n")
      (insert (format "%S\n" args))
      (insert "EGG-GIT-OUTPUT:\n")
      (setq proc (apply 'start-process proc-name (current-buffer) command args))
      (setq mode-line-process " git")
      (when (and (consp func-args) (functionp (car func-args)))
	(process-put proc :callback-func (car func-args))
	(process-put proc :callback-args (cdr func-args)))
      (when (stringp accepted-msg)
	(process-put proc :accepted-msg accepted-msg)
	(process-put proc :accepted-code exit-code))
      (process-put proc :cmds (cons command args))
      (set-process-sentinel proc #'egg-process-sentinel))
    proc))

(defsubst egg-buffer-async-do (accepted-code &rest args)
  "Run git asynchronously and refresh the current buffer on exit.
exit code ACCEPTED-CODE is considered a success."
  (egg-async-do accepted-code
                (cons (or egg-buffer-async-cmd-refresh-func
                          egg-buffer-refresh-func)
                      (list (current-buffer)))
                args))

(defsubst egg-async-0 (func-args &rest args)
  (egg-async-do nil func-args args))

(defsubst egg-async-1 (func-args &rest args)
  (egg-async-do 1 func-args args))

(defsubst egg-async-0-args (func-args args)
  (egg-async-do 0 func-args args))

(defsubst egg-async-1-args (func-args args)
  (egg-async-do 1 func-args args))

(defvar egg-async-process nil)
(defvar egg-async-cmds nil)
(defvar egg-async-exit-msg nil)
(defvar egg-async-exit-status nil)

(defun egg-process-sentinel (proc msg)
  (let ((exit-code (process-get proc :accepted-code))
        (accepted-msg (process-get proc :accepted-msg))
        (callback-func (process-get proc :callback-func))
        (callback-args (process-get proc :callback-args))
        (cmds (process-get proc :cmds))
	status)
    (cond ((string= msg "finished\n")
           (message "EGG: git finished.")
	   (setq status :finished))
          ((string= msg "killed\n")
           (message "EGG: git was killed.")
	   (setq status :killed))
          ((and accepted-msg (string-match accepted-msg msg))
           (message "EGG: git exited with code: %d." exit-code)
	   (setq status exit-code))
          ((string-match "exited abnormally" msg)
           (message "EGG: git failed.")
	   (setq status :failed))
          (t (message "EGG: git is weird!")
	     (setq status :confused)))
    (with-current-buffer (process-buffer proc)
      (setq mode-line-process nil)
      (widen)
      (egg-cmd-log-whole-buffer (current-buffer))
      (goto-char (point-max))
      (re-search-backward "^EGG-GIT-CMD:" nil t)
      ;; Narrow to the last command
      (narrow-to-region (point) (point-max))
      (if (functionp callback-func)
          (let ((egg-async-process proc)
                (egg-async-cmds cmds)
                (egg-async-exit-msg msg)
		(egg-async-exit-status status))
            (apply callback-func callback-args))))))


(defun egg-cmd-log-buffer ()
  (or (get-buffer (concat " *egg-cmd-logs@" (egg-git-dir) "*"))
      (let ((git-dir (egg-git-dir))
            (default-directory default-directory)
            dir)
        (unless git-dir
          (error "Can't find git dir in %s" default-directory))
        (setq dir (egg-work-tree-dir git-dir))
        (setq default-directory dir)
        (get-buffer-create (concat " *egg-cmd-logs@" git-dir "*")))))

(defun egg-cmd-log (&rest strings)
  (with-current-buffer (egg-cmd-log-buffer)
    (goto-char (point-max))
    (cons (current-buffer)
          (prog1 (point)
            (apply 'insert-before-markers "LOG/" strings)
	    (goto-char (point-max))))))

(defun egg-cmd-log-whole-buffer (buffer)
  (with-current-buffer (egg-cmd-log-buffer)
    (goto-char (point-max))
    (cons (current-buffer)
          (prog1 (point)
	    (insert-buffer-substring buffer)
	    (goto-char (point-max))))))


(defun egg-previous-non-hidden (pos)
  (while (and (> pos (point-min)) (invisible-p pos))
    (setq pos (previous-single-property-change pos 'invisible)))
  pos)

(defun egg-refresh-buffer (buffer)
  (when (and (bufferp buffer) (buffer-live-p buffer))
    (with-current-buffer buffer
	(when (and (egg-git-dir)
		   (functionp egg-buffer-refresh-func))
	  (let ((line (count-lines (point-min) (point)))
		(column (current-column))
		(anchor (get-text-property (point) :navigation))
		(offset (egg-section-relative-pos (point)))
		(win-anchor-off-line-col-alist
		 (mapcar (lambda (win)
			   (let* ((win-pos (window-point win))
				  (win-anchor (get-text-property win-pos :navigation))
				  (win-off (egg-section-relative-pos win-pos))
				  (win-line (count-lines (point-min) win-pos))
				  (win-col (save-excursion
					     (goto-char win-pos)
					     (current-column))))
			     (list win win-anchor win-off win-line win-col)))
			 (get-buffer-window-list))))
	    (funcall egg-buffer-refresh-func (current-buffer))
	    (if anchor
		(egg-buffer-goto-section anchor)
	      (egg-goto-line line)
	      (goto-char (egg-previous-non-hidden (+ (line-beginning-position) column))))
	    (dolist (win-anchor-off-line-col win-anchor-off-line-col-alist)
	      (let ((win (nth 0 win-anchor-off-line-col))
		    (anchor (nth 1 win-anchor-off-line-col))
		    (offset (nth 2 win-anchor-off-line-col))
		    (line (nth 3 win-anchor-off-line-col))
		    (col (nth 4 win-anchor-off-line-col)))
		(with-selected-window win
		  (if anchor
		      (egg-buffer-goto-section anchor offset)
		    (egg-goto-line line)
		    (goto-char (egg-previous-non-hidden
				(+ (line-beginning-position) column))))))))))))

;;;========================================================
;;; New: internal command
;;;========================================================

(defsubst egg--do-output (&optional erase)
  "Get the output buffer for synchronous commands.
erase the buffer's contents if ERASE was non-nil."
  (let ((buffer (get-buffer-create (concat " *egg-output:" (egg-git-dir) "*")))
	(default-directory default-directory))
    (with-current-buffer buffer
      (setq default-directory (egg-work-tree-dir default-directory))
      (widen)
      (if erase (erase-buffer)))
    buffer))

(defmacro with-egg--do-buffer (&rest body)
  "Evaluate BODY there like `progn' in the egg--do-output buffer.
See also `with-temp-file' and `with-output-to-string'."
  (declare (indent 0) (debug t))  
  `(with-current-buffer (egg--do-output)
     (setq default-directory (egg-work-tree-dir default-directory))
     (unwind-protect
	 (progn ,@body)
       )))

(defmacro with-clean-egg--do-buffer (&rest body)
  "Evaluate BODY there like `progn' in the egg--do-output buffer.
See also `with-temp-file' and `with-output-to-string'."
  (declare (indent 0) (debug t))  
  `(with-current-buffer (egg--do-output t)
     (let ((process-environment (egg--git-check-index)))
       (setq default-directory (egg-work-tree-dir default-directory))
       (unwind-protect
	   (progn ,@body)
	 ))))

(defun egg--do (stdin program args &optional no-log)
  "Run PROGRAM with ARGS synchronously using STDIN as starndard input.
ARGS should be a list of arguments for PROGRAM."
  (let ((buf (current-buffer))
	ret)
    (unless no-log
      (egg-cmd-log "RUN:" program " " (mapconcat 'identity args " ")
		   (if stdin " <REGION\n" "\n")))
    (with-clean-egg--do-buffer
      (cond ((stringp stdin)
	     (insert stdin))
	    ((consp stdin)
	     (insert-buffer-substring buf (car stdin) (cdr stdin)))
	    (t nil))

      (setq ret (if stdin
		    (apply 'call-process-region (point-min) (point-max)
			   program t t nil args)
		  (apply 'call-process program nil t nil args)))
      (unless no-log
	(egg-cmd-log-whole-buffer (current-buffer))
	(egg-cmd-log (format "RET:%d\n" ret)))
      (cons ret (current-buffer)))))

(defun egg--do-git (stdin cmd args &optional no-log)
  "Run git command CMD with ARGS synchronously, using STDIN as starndard input.
ARGS should be a list of arguments for the git command CMD."
  (egg--do stdin egg-git-command (cons cmd args) no-log))

(defun egg--do-handle-exit (exit-info post-proc-func &optional buffer-to-update)
  "Handle the exit code and the output of a synchronous action.
EXIT-INFO is the results of the action in form of a pair (return-code . output-buffer).
POST-PROC-FUNC shall be a function which will be call with 1 argument: the return-code
of the action. It shall be called in the output buffer of the action.
This function returns the returned value of POST-PROC-FUNC.
EXIT-INFO should be the return value of `egg--do-git' or `egg--do'."
  (let ((ret (car exit-info))
	(buf (cdr exit-info))
	beg end pp-results)
    (with-current-buffer buf
      (setq beg (point-min))
      (setq end (point-max))
      ;; even if the buffer is empty, post-proc-func must
      ;; still be done to process the ret-code
      (setq pp-results
	    (cond ((functionp post-proc-func)
		   (goto-char (point-min))
		   (funcall post-proc-func ret))
		  ((and (consp post-proc-func)
			(functionp (car post-proc-func))
			(functionp (cdr post-proc-func)))
		   (funcall (cdr post-proc-func) (funcall (car post-proc-func) ret))))))
    (cond ((bufferp buffer-to-update)
	   (egg-refresh-buffer buffer-to-update))
	  ((memq buffer-to-update '(t all))
	   (egg-run-buffers-update-hook))
	  (t nil))
    (if (memq :success pp-results)
	pp-results
      (nconc (list :success (= ret 0)) pp-results)		;; default result
      )))

(defvar egg--do-git-quiet nil)		;; don't show git's output
(defvar egg--do-no-output-message nil)

(defun egg--do-show-output (cmd-name output-info)
  "Show the output of a synchronous git command as feed-back for the emacs command.
CMD-NAME is the name of the git command such as: merge or checkout. OUTPUT-INFO is
generally the returned value of `egg--do-handle-exit'. OUTPUT-INFO will also be
used as the returned value of this function."
  (let ((ok (plist-get output-info :success))
	(line (plist-get output-info :line))
	(no-output-message (or egg--do-no-output-message "*no output*"))
	prefix)
    (setq prefix (concat (if (stringp cmd-name) cmd-name "GIT")
			 (if ok "> " ":ERROR> ")))
    (unless (and egg--do-git-quiet ok)
      (message (if (stringp line) 
		   (concat prefix line)
		 (concat "EGG: " no-output-message))))
    (unless ok (ding))
    output-info))

(defun egg--do-git-action (cmd buffer-to-update post-proc-func args &optional no-log)
  "Run git command CMD with arguments list ARGS.
Show the output of CMD as feedback of the emacs command.
Update the buffer BUFFER-TO-UPDATE and use POST-PROC-FUNC as the
output processing function for `egg--do-handle-exit'."
  (egg--do-show-output (concat "GIT-" (upcase cmd))
		       (egg--do-handle-exit (egg--do-git nil cmd args no-log)
					    post-proc-func buffer-to-update)))

(defun egg--do-git-action-stdin (cmd stdin buffer-to-update post-proc-func args &optional no-log)
  "Run git command CMD with arguments list ARGS and STDIN as standard input.
Show the output of CMD as feedback of the emacs command.
Update the buffer BUFFER-TO-UPDATE and use POST-PROC-FUNC as the
output processing function for `egg--do-handle-exit'."
  (egg--do-show-output (concat "GIT-" (upcase cmd))
		       (egg--do-handle-exit (egg--do-git stdin cmd args no-log)
					    post-proc-func buffer-to-update)))



(defconst egg--bad-git-output-regex
  (concat (regexp-opt '("Cannot" "cannot" "Couldn't" "couldn't" "Could not" "could not" 
			"Failed" "failed" "incompatible" "Incompatible" "invalid" "Invalid"
			"not allowed" "rejected" "Unable" "unable" "internal error" 
			"mutually exclusive" "does not" "do not" "did not" "is not" "needs a"
			"No such" "no such" "No changes" "Too many" "too many"
			"Nothing" "nothing" "Abort" "abort" "Malformed" "malformed"
			"unresolved" "Unresolved" "Corrupt" "corrupt" "empty" 
			"does not make sense" "only with" "only one" "only allowed"
			"skipped" "Skipped" "bad" "Bad" "doesn't"
			"too big" "Too big" "too many" "Too many"
			"not a valid" "Not a valid" "already exist" "ignored by" "is beyond"

			"Not"
			) t)
	  "\\|\\(?:No.+found\\)\\|\\(?:[On]nly.+can be used\\)"))

(defconst egg--fatal-git-output-regex "^fatal")

(defun egg--git-pp-grab-line-no (line-no &rest extras)
  "Grab the line LINE-NO in the current buffer.
Return it in a form usable for `egg--do-show-output'."
  (let* ((lines (delete "" (save-match-data
			     (split-string (buffer-string) "\n"))))
	 (line (cond ((< line-no 0) (nth (- -1 line-no) (nreverse lines)))
		     ((> line-no 0) (nth (1- line-no) lines))
		     (t nil))))
    (when (stringp line)
      (nconc (list :line line) extras))))

(defun egg--git-pp-grab-line-matching (regex &optional replacement &rest extras)
  "If REGEX matched a line in the current buffer, return it in a form suitable
for `egg--do-show-output'. If REPLACEMENT was provided, use it in the returned
structure instead of the matching line."
  (let* ((case-fold-search nil)
	 (matched-line 
	 (when (stringp regex)
	   (save-match-data
	     (goto-char (point-min))
	     (when (re-search-forward regex nil t)
	       (goto-char (match-beginning 0))
	       (buffer-substring-no-properties 
		(line-beginning-position)
		(line-end-position)))))))
    (when (stringp matched-line)
      (nconc (list :line (if (stringp replacement) replacement matched-line))
	     extras))))

(defun egg--git-pp-grab-line-matching-backward (regex &optional replacement &rest extras)
  "If REGEX matched a line in the current buffer, return it in a form suitable
for `egg--do-show-output'. If REPLACEMENT was provided, use it in the returned
structure instead of the matching line."
  (let* ((case-fold-search nil)
	 (matched-line 
	 (when (stringp regex)
	   (save-match-data
	     (goto-char (point-max))
	     (when (re-search-backward regex nil t)
	       (goto-char (match-beginning 0))
	       (buffer-substring-no-properties 
		(line-beginning-position)
		(line-end-position)))))))
    (when (stringp matched-line)
      (nconc (list :line (if (stringp replacement) replacement matched-line))
	     extras))))

(defsubst egg--git-pp-grab-1st-line-matching (regex-list &optional replacement &rest extras)
  "Try maching the lines in the current buffer against each regex in REGEX-LIST
until one matched. Return the line in a form suitable for `egg--do-show-output'.
If REPLACEMENT was provided, use it in the returned structure instead of
the matching line."
  (dolist-done (re regex-list found)
    (setq found (apply 'egg--git-pp-grab-line-matching re replacement extras))))

(defun egg--git-pp-fatal-output (&optional pre-regexes post-regexes)
  (or
   (egg--git-pp-grab-1st-line-matching
    (nconc (if (stringp pre-regexes) (list pre-regexes) pre-regexes)
	   (list egg--bad-git-output-regex egg--fatal-git-output-regex)
	   (if (stringp post-regexes) (list post-regexes) post-regexes)))
   (egg--git-pp-grab-line-no -1)))

(defsubst egg--git-pp-fatal-result (&optional pre-regexes post-regexes)
  (nconc (list :success nil)
	 (egg--git-pp-fatal-output pre-regexes post-regexes)))

(defun egg--git-pp-generic (ret-code accepted-codes ok-regex bad-regex
				     &optional line-no)
  "Simple post-processing function for synchronous git actions.
return a suitable structure for `egg--do-show-output'. if RET-CODE was 0
then the action is considered a success. The line matching OK-REGEX would
be also return in the structure. OK-REGEX can also be a list of regexes.
If no line matched OK-REGEX and LINE-NO was provided, then return the line
LINE-NO in the result structure. If RET-CODE wasn't 0, then use BAD-REGEX
instead of OK-REGEX.

This simple function might be use as the POST-PROC-FUNC argument of the
`egg--do-handle-exit' function."
  (if (memq ret-code accepted-codes)
      (if (consp ok-regex)
	  (egg--git-pp-grab-1st-line-matching ok-regex nil :success t)
	(egg--git-pp-grab-line-matching ok-regex nil :success t))
    (or (if (consp bad-regex)
	    (egg--git-pp-grab-1st-line-matching bad-regex)
	  (egg--git-pp-grab-line-matching bad-regex))
	(egg--git-pp-grab-line-no (or line-no -1)))))

(defconst egg--git-action-cmd-doc nil
  "The `egg--git-xxxxx-cmd' functions perform git command synchronously.
The returned value is a plist where the results of the action are indexed
by the :aaaaa symbols

:success (t or nil) the success of the action from egg's p-o-v.
:line the selected line from the git command's output to display
      to the user as the feedback of the emacs command. Sometimes
      egg fabricates this line.
:files the files in the worktree which might be changed by the
       git command and their buffers should be reverted.
:next-action (a symbol) the next logical action for egg to do.")

(defun egg--git-push-cmd (buffer-to-update &rest args)
  "Peform a synchronous action using the git push command using ARGS as arguments.
Update BUFFER-TO-UPDATE if needed.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (egg--do-git-action 
   "push" buffer-to-update
   (lambda (ret-code)
     (egg--git-pp-generic ret-code '(0) " -> \\|Everything up-to-date\\|deleted"
			  '("rejected" "\\<not\\>")))
   args))

(defun egg--git-push-cmd-test (from to repo)
  (interactive "sPush: \nsOnTo: \nsAt:")
  (egg--git-push-cmd nil repo (concat from ":" to)))

(defun egg--git-pp-reset-output (ret-code reset-mode)
  "Post-processing function for the output the git reset command in the current buffer.
RET-CODE is the return code of the git process and RESET-MODE would be one
of: --hard, --keep, --soft, --mixed. --merge is currently not supported.
Return a structure suitable for `egg--do-show-output'."
  (if (/= ret-code 0)
      (egg--git-pp-fatal-output "Entry.not uptodate")
    (cond ((string-equal "--hard" reset-mode)
	   (egg--git-pp-grab-line-matching "^HEAD is now at" nil
					   :next-action 'log :success t))
	  ((string-equal "--keep" reset-mode)
	   (egg--git-pp-grab-line-no -1 :next-action 'status :success t))
	  ((string-equal "--soft" reset-mode)
	   (egg--git-pp-grab-line-no -1 :next-action 'status :success t))
	  ((string-equal "--mixed" reset-mode)
	   (egg--git-pp-grab-line-matching "Unstaged changes after reset"
					   "there are unstaged changes after reset"
					   :next-action 'status :success t)))))

(defun egg--git-reset-cmd (buffer-to-update reset-mode rev)
  "Peform a synchronous action using the git reset command.
Update BUFFER-TO-UPDATE if needed. RESET mode is a one of: --hard, --keep, --mixed
and --soft (--merge is currently unsupported.). HEAD will be resetted to REV.
The relevent line from the output of the underlying git command will be display
as feedback of emacs command.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (let ((pre-reset (egg-get-current-sha1))
	;; will be changed, if "--keep"
	(rev-vs-head (egg-git-to-lines "diff" "--name-only" "HEAD" rev))
	;; will be changed, if "--hard"
	(rev-vs-wdir (egg-git-to-lines "diff" "--name-only" rev))
	files res)
    (setq res (egg--do-git-action
	       "reset" buffer-to-update
	       `(lambda (ret-code)
		  (egg--git-pp-reset-output ret-code ,reset-mode))
	       (list reset-mode rev)))
    (if (plist-get res :success)
	(cond ((equal reset-mode "--hard")
	       (nconc res (list :files rev-vs-wdir)))
	      ((member reset-mode '("--keep" "--merge"))
	       (nconc res (list :files rev-vs-head)))
	      (t res))
      res)))

(defun egg--git-reset-cmd-test (mode rev)
  (interactive "sreset mode:\nsrev:")
  (egg--git-reset-cmd t mode rev))

(defun egg--git-reset-files-cmd (buffer-to-update rev &rest files)
  "Peform a synchronous action using the git reset command on paths FILES.
Update BUFFER-TO-UPDATE if needed. FILES will be resetted to REV in the index.
The relevent line from the output of the underlying git command will be displayed
as feedback of emacs command.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (egg--do-git-action
   "reset" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0) (list :success t :next-action 'status))
	   ((= ret-code 1)
	    (egg--git-pp-grab-line-matching "Unstaged changes after reset:"
					    "there are unstaged changes after reset"
					    :next-action 'status :success t))
	   ((> ret-code 1) (egg--git-pp-fatal-result))))
   (nconc (list (or rev "HEAD") "--") files)))


(defun egg--git-co-files-cmd (buffer-to-update file-or-files &rest args)
  "Peform a synchronous action using the git checkout command on FILE-OR-FILES.
Update BUFFER-TO-UPDATE if needed. ARGS will be passed to the git command as
arguments. FILE-OR-FILES will be updated to REV in the index as well as the work
tree. The relevent line from the output of the underlying git command will be
displayed as feedback of emacs command.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (let* ((files (if (consp file-or-files) file-or-files (list file-or-files)))
	 (args (append args (cons "--" files)))
	 (res (egg--do-git-action
	       "checkout" buffer-to-update
	       (lambda (ret-code)
		 (if (= ret-code 0)
		     (list :success t :next-action 'status)
		   (egg--git-pp-fatal-result "yet to be born")))
	       args)))
    (if (plist-get res :success)
	(nconc res (list :files files))
      res)))

(defun egg--git-co-rev-cmd-args (buffer-to-update rev args)
  "Peform a synchronous action using git to checkout REV to the worktree.
ARGS will be used as arguments. Update BUFFER-TO-UPDATE if
needed. The relevent line from the output of the underlying git
command will be displayed as feedback of emacs command."
  (let (files cmd res)
    (if (eq :0 rev)
	(setq files ;; index vs wdir
	      (egg-git-to-lines "diff" "--name-only")
	      cmd "checkout-index")
      ;; will change if switch rev
      (setq files (egg-git-to-lines "diff" "--name-only" rev "HEAD")
	    cmd "checkout"
	    args (append args (list rev))))
    (setq res 
	  (egg--do-git-action 
	   cmd buffer-to-update 
	   (lambda (ret-code)
	     (if (= ret-code 0)
		 (or (egg--git-pp-grab-line-matching 
		      (regexp-opt '("Already on" "HEAD is now at"
				    "Switched to branch" "Switched to a new branch"
				    "Reset branch")) nil
		      :next-action 'status :success t)
		     (egg--git-pp-grab-line-no -1 :next-action 'status :success t))
	       (or
		(egg--git-pp-grab-line-matching "untracked working tree files would be overwritten"
						"untracked file(s) would be overwritten"
						:next-action 'status)
		(egg--git-pp-fatal-result "Please, commit your changes"))))
	   args))
    
    (if (plist-get res :success)
	(nconc res (list :files files))
      res)))

(defsubst egg--git-co-rev-cmd (buffer-to-update rev &rest args)
  "Peform a synchronous action using git to checkout REV to the worktree.
ARGS will be used as arguments. Update BUFFER-TO-UPDATE if
needed. The relevent line from the output of the underlying git
command will be displayed as feedback of emacs command.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (egg--git-co-rev-cmd-args buffer-to-update rev args))

(defun egg--git-merge-pp-func (ret-code)
  (cond ((= ret-code 0)
	 (or (egg--git-pp-grab-line-matching "stopped before committing as requested"
					     nil :next-action 'commit :success t)
	     (egg--git-pp-grab-line-matching
	      (regexp-opt '("merge went well" "Already up-to-date" 
			    "file changed" "files changed"
			    "insertions" "deletions"))
	      nil :success t :next-action 'log)
	     (egg--git-pp-grab-line-no -1 :success t :next-action 'status)))
	((= ret-code 1)
	 (or (egg--git-pp-grab-line-matching "fix conflicts and then commit"
					     nil :next-action 'status :success t)
	     (egg--git-pp-grab-line-matching
	      "untracked working tree files would be overwritten by merge"
	      "untracked files would be overwritten, please rename them before merging"
	      :next-action 'status)
	     (egg--git-pp-grab-line-matching
	      "commit your changes or stash them before you can merge"
	      nil :next-action 'status)
	     (egg--git-pp-grab-line-no -1)))
	(t (egg--git-pp-fatal-result))))

(defun egg--git-merge-cmd-args (buffer-to-update pp-func &optional args)
  "Peform the git merge command synchronously with ARGS as arguments.
Update BUFFER-TO-UPDATE if needed. The relevant line from the
output of the underlying git command will be displayed as
feedback of emacs command.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (egg--do-git-action 
   "merge" buffer-to-update
   (if (functionp pp-func)
       (cons #'egg--git-merge-pp-func pp-func)
     #'egg--git-merge-pp-func)
   args))

(defun egg--git-merge-cmd (buffer-to-update pp-func &rest args)
  (egg--git-merge-cmd-args buffer-to-update pp-func args))

(defun egg--git-merge-cmd-test (ff-only from)
  (interactive "P\nsMerge: ")
  (if ff-only
      (egg--git-merge-cmd t "-v" "--ff-only" from)
    (egg--git-merge-cmd t "-v" from)))


(defun egg--git-rebase-merge-pp-func (ret-code)
  (cond ((= ret-code 0)
	 (or (egg--git-pp-grab-line-matching "All done" "rebase was successful!"
					     :success t :next-action 'log)
	     (egg--git-pp-grab-line-matching "HEAD is now at" ;; --abort
					     :success t :next-action 'log)
	     (egg--git-pp-grab-line-matching "Fast-forwarded" ;; onto is proper descendant
					     :success t :next-action 'log)
	     (egg--git-pp-grab-line-no -1 :success t :next-action 'log)))
	(t
	 (or (egg--git-pp-grab-line-matching "You still have unmerged paths in your index"
					     "please add conflict resolution(s) into index"
					     :success t :next-action 'status)
	     (egg--git-pp-grab-line-matching "Commit failed, please do not call"
					     "troubles! failed to commit this cherry"
					     :success nil :next-action 'status)
	     (egg--git-pp-grab-1st-line-matching 
	      '("When you have resolved this problem" 
		"Strategy: .+ failed, try another"
		"You must edit all merge conflicts"
		"error: could not apply"
		"Automatic cherry-pick failed")
	      "please resolve merge conflicts"
	      :success t :next-action 'status)
	     (egg--git-pp-grab-1st-line-matching
	      '("You are not currently on a branch" "There is no tracking information")
	      "rebase failed to determine upstream for the current branch"
	      :success nil :next-action 'log)
	     (egg--git-pp-grab-1st-line-matching
	      '("there are more than one merge bases" 
		"there is no merge base"
		"Does not point to a valid commit") nil
		:success nil :next-action 'log)
	     (egg--git-pp-grab-line-matching
	      "I wonder if you are in the middle of another rebase"
	      "a rebase is already in progress!"
	      :success nil :next-action 'status)
	     (egg--git-pp-grab-1st-line-matching 
	      '("The pre-rebase hook refused to rebase" "No rebase in progress"
		"fatal: no such branch:")
	      nil :success nil :next-action 'log)
	     (egg--git-pp-fatal-result)))))

(defun egg--git-rebase-merge-cmd-args (buffer-to-update pp-func args)
  "Peform the git rebase -m command synchronously with ARGS as arguments.
Update BUFFER-TO-UPDATE if needed. The relevant line from the
output of the underlying git command will be displayed as
feedback of emacs command.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (egg--do-git-action 
   "rebase" buffer-to-update
   (if (functionp pp-func)
       (cons #'egg--git-rebase-merge-pp-func pp-func)
     #'egg--git-rebase-merge-pp-func)
   args))

(defsubst egg--git-rebase-merge-cmd (buffer-to-update pp-func &rest args)
  "Peform the git rebase -m command synchronously with ARGS as arguments.
Update BUFFER-TO-UPDATE if needed. The relevant line from the
output of the underlying git command will be displayed as
feedback of emacs command.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (egg--git-rebase-merge-cmd-args buffer-to-update pp-func args))


(defun egg--git-add-cmd (buffer-to-update &rest args)
  "Run git add command synchronously with ARGS as arguments.
Update BUFFER-TO-UPDATE as needed.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (egg--do-git-action
   "add" buffer-to-update
   (lambda (ret-code)
     (if (= ret-code 0)
	 (or (egg--git-pp-grab-line-matching "nothing added" nil :success t)
	     (egg--git-pp-grab-line-matching "^add " "index updated" 
					     :next-action 'status :success t)
	     (egg--git-pp-grab-line-no -1 :success t))
       (egg--git-pp-fatal-result)))
   args))

(defun egg--git-rm-cmd (buffer-to-update &rest args)
  "Run the git rm command synchronously with ARGS as arguments.
Update BUFFER-TO-UPDATE as needed.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (egg--do-git-action
   "rm" buffer-to-update
   (lambda (ret-code)
     (if (= ret-code 0)
	 (nconc (list :success t)
		(let (files)
		  (goto-char (point-min))
		  (while (re-search-forward "^rm '\\(.+\\)'$" nil t)
		    (add-to-list 'files (match-string-no-properties 1)))
		  (when (consp files)
		    (list :files files :next-action 'status))))
       (egg--git-pp-fatal-result "has staged content different from both"
				 (regexp-opt '("did not match" "not removing")))))
   args))


(defun egg--git-branch-cmd (buffer-to-update args)
  "Run the git branch command synchronously with ARGS as arguments.
Update BUFFER-TO-UPDATE as needed.

See documentation of `egg--git-action-cmd-doc' for the return structure."  (egg--do-git-action
   "branch" buffer-to-update
   (lambda (ret-code)
     (if (= ret-code 0)
	  (egg--git-pp-grab-line-no -1 :success t)
       (egg--git-pp-fatal-result (regexp-opt '("No commit" "no commit" 
					       "No such" "no such" "is not fully")))))
   args))

(defconst egg--git-stash-error-regex
  "unimplemented\\|\\([dD]o \\|[Cc]ould \\|[Cc]an\\)not\\|No \\(changes\\|stash found\\)\\|Too many\\|is not\\|unable\\|Conflicts in index\\|No branch name\\|^fatal")

(defun egg--git-stash-save-cmd (buffer-to-update &rest args)
  "Run the git stash save command synchronously with ARGS as arguments.
Update BUFFER-TO-UPDATE as needed.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (let ((files (egg-git-to-lines "diff" "--name-only" "HEAD"))
	res)
    (setq res
	  (egg--do-git-action
	   "stash" buffer-to-update
	   (lambda (ret-code)
	     (if (= ret-code 0)
		 (or (egg--git-pp-grab-1st-line-matching
		      '("Saved working directory" "HEAD is now at") nil
		      :next-action 'stash :success t)
		     (egg--git-pp-grab-line-no -1 :next-action 'stash :success t))
	       (egg--git-pp-fatal-result)))
	   (cons "save" args)))
    (when (plist-get res :success)
      (setq res (nconc res (list :files files))))
    res))

(defun egg--git-stash-drop-cmd (buffer-to-update &rest args)
  "Run the git stash save command synchronously with ARGS as arguments.
Update BUFFER-TO-UPDATE as needed.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (egg--do-git-action
   "stash" buffer-to-update
   (lambda (ret-code)
     (if (= ret-code 0)
	 (or (egg--git-pp-grab-line-matching "^Dropped stash" nil :next-action 'stash :success t)
	     (egg--git-pp-grab-line-no -1 :next-action 'stash :success t))
       (or (egg--git-pp-grab-line-matching "only has" nil :next-action 'stash)
	   (egg--git-pp-fatal-result))))
   (cons "drop" args)))

(defun egg--git-stash-unstash-cmd (buffer-to-update cmd &optional args)
  "Run a git stash CMD command synchronously with ARGS as arguments.
CMD should be pop, apply or branch.
 See documentation of `egg--git-action-cmd-doc' for the return structure."
  (unless (egg-has-stashed-wip)
    (error "no WIP was stashed!"))
  (let ((files (egg-git-to-lines "diff" "--name-only" "stash@{0}"))
	(cmd (or cmd "pop"))
	res)
    (setq res
	  (egg--do-git-action
	   "stash" buffer-to-update
	   (lambda (ret-code)
	     (if (= ret-code 0)
		 (or (egg--git-pp-grab-line-matching "Dropped refs/stash" nil
						     :next-action 'status :success t)
		     (egg--git-pp-grab-line-no -1 :next-action 'status :success t))
	       (or (egg--git-pp-grab-line-matching "^CONFLICT" nil 
						   :next-action 'status :success t)
		   (egg--git-pp-grab-line-matching "Conflicts in index. Try without --index"
						   :next-action 'status :success nil)
		   (egg--git-pp-grab-line-matching "^error: patch failed:"
						   :next-action 'status :success nil)
		   (egg--git-pp-grab-line-matching 
		    "following files would be overwritten"
		    "stashed wip conflicts with local modifications, please commit first"
		    :next-action 'status)
		   (egg--git-pp-fatal-result))))
	   (cons cmd args)))
    (when (plist-get res :success)
      (setq res (nconc res (list :files files))))
    res))

(defun egg--git-cherry-pick-pp (ret-code rev not-commit-yet)
  (cond ((= ret-code 0)
	 (or (egg--git-pp-grab-line-matching
	      (regexp-opt '("file changed" "files changed"
			    "insertions" "deletions"))
	      nil :success t :next-action (if not-commit-yet 'commit 'log))
	     (if not-commit-yet
		(if (egg-git-ok nil "diff" "--quiet" "--cached")
		    ;; cherry pick produced empty index
		    (list :success t
			  :line (or (egg--git-pp-grab-line-no -1)
				    (format "cherry '%s' evaporated!!!" (egg-sha1 rev))))
		  (list :success t :next-action 'commit
			:line (or (egg--git-pp-grab-line-no -1)
				  (format "cherry '%s' picked, ready to be committed"
					  (egg-sha1 rev)))))
	       (list :success t :next-action 'log
		     :line (or (egg--git-pp-grab-line-no -1)
			       (format "'%s' applied cleanly" (egg-sha1 rev)))))))
	((= ret-code 1)
	 (or
	  (egg--git-pp-grab-line-matching 
	   (regexp-opt '("cherry-pick is now empty" "nothing to commit"))
	   nil :success t)
	  (egg--git-pp-grab-line-matching "after resolving the conflicts"
					  "please resolve conflicts"
					  :next-action 'status :success t)
	  (egg--git-pp-grab-line-matching "error: could not apply" nil
					  :next-action 'status)
	  (egg--git-pp-grab-line-no -1)))
	(t (egg--git-pp-fatal-result))))

(defun egg--git-cherry-pick-cmd (buffer-to-update rev &rest args)
  "Run the git cherry-pick command synchronously with ARGS as arguments.
REV is the commit to be picked.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (let ((files (egg-git-to-lines "diff" "--name-only" rev))
	(no-commit (and (member "--no-commit" args) t))
	res)
    (setq res 
	  (egg--do-git-action
	   "cherry-pick"
	   buffer-to-update
	   `(lambda (ret-code)
	      (egg--git-cherry-pick-pp ret-code ,rev ,no-commit))
	   (nconc args (list rev))))
    (when (plist-get res :success)
      (nconc res (list :files files)))
    res))

;; (defun egg--git-cherry-pick-cmd-test (rev option)
;;   (interactive
;;    (list (egg-read-rev "cherry pick rev: " 
;; 		       (or (egg-ref-at-point) (egg-commit-at-point) (egg-string-at-point)))
;; 	 (read-string "cherry pick option: " "--no-commit")))
;;   (egg--buffer-handle-result (egg--git-cherry-pick-cmd t rev option) t))

(defun egg--git-apply-cmd (buffer-to-update patch &optional args)
  "Run the git apply command with PATCH as input and ARGS as arguments.
Update BUFFER-TO-UPDATE as needed.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (let ((files (egg-git-lines-matching-stdin patch "^[0-9]+\t[0-9]+\t\\(.+\\)$" 1
					     "apply" "--numstat" "-"))
	res)
    (setq res 
	  (egg--do-git-action-stdin
	   "apply" patch
	   buffer-to-update
	   (lambda (ret-code)
	     (cond ((= ret-code 0)
		    (or (egg--git-pp-grab-line-matching "Applied patch.+cleanly" 
							"patch applied cleanly"
							:success t :next-action 'commit)
			(egg--git-pp-grab-line-no -1 :success t :next-action 'status)))
		   ((= ret-code 1) 
		    (or (egg--git-pp-grab-line-matching "Fall back to three-way merge"
							"Patch produced conflicts"
							:success t :next-action 'status)
			(egg--git-pp-grab-1st-line-matching 
			 '("Apply patch to.+with conflicts"
			   "error:.+patch does not apply"
			   "patch failed:"))
			(egg--git-pp-grab-line-no -1)))
		   (t (egg--git-pp-fatal-result))))
	   (append args (list "-v" "-"))))
    (when (plist-get res :success)
      (nconc res (list :files files)))
    res))

;; (defun egg--git-apply-cmd-test (file)
;;   (interactive "fpatch file: ")
;;   (with-temp-buffer
;;     (insert-file-contents-literally file)
;;     (egg--git-apply-cmd nil (buffer-string) '("-3"))
;;     (egg-status nil nil)))


(defun egg--git-revert-pp (ret-code rev not-commit-yet)
  (cond ((= ret-code 0)
	 (or (egg--git-pp-grab-line-matching
	      (regexp-opt '("file changed" "files changed"
			    "insertions" "deletions"))
	      nil :success t :next-action (if not-commit-yet 'commit 'log))
	     (if not-commit-yet
		(if (egg-git-ok nil "diff" "--quiet" "--cached")
		    ;; revert produced empty index
		    (list :success t
			  :line (or (egg--git-pp-grab-line-no -1)
				    (format "successfully reverted '%s', but resulted in no changes"
					    (egg-sha1 rev))))
		  (list :success t :next-action 'commit
			:line (or (egg--git-pp-grab-line-no -1)
				  (format "reverted '%s', ready to commit"
					  (egg-sha1 rev)))))
	       (list :success t :next-action 'log
		     :line (or (egg--git-pp-grab-line-no -1)
			       (format "rev '%s' reverted" (egg-sha1 rev)))))))
	((= ret-code 1)
	 (or
	  (egg--git-pp-grab-line-matching "after resolving the conflicts"
					  "revert produced conflicts, please resolve"
					  :next-action 'status :success t)
	  (egg--git-pp-grab-line-matching "error: could not revert" nil
					  :next-action 'status)
	  (egg--git-pp-grab-line-no -1)))
	(t (egg--git-pp-fatal-result))))

(defun egg--git-revert-cmd (buffer-to-update rev use-default-msg)
  "Run the git revert command synchronously with ARGS as arguments.
REV is the commit to be picked.

See documentation of `egg--git-action-cmd-doc' for the return structure."
  (let ((files (egg-git-to-lines "diff" "--name-only" rev))
	(not-commit-yet (null use-default-msg))
	res)
    (setq res 
	  (egg--do-git-action
	   "revert"
	   buffer-to-update
	   `(lambda (ret-code)
	      (egg--git-revert-pp ret-code ,rev ,not-commit-yet))
	   (list (if use-default-msg "--no-edit" "--no-commit") rev)))
    (when (plist-get res :success)
      (nconc res (list :files files)))
    res))

(defun egg--git-tag-cmd-pp (ret-code)
  (cond ((= ret-code 0)
	 (or (egg--git-pp-grab-1st-line-matching 
	      '("Deleted tag" "Updated tag" "Good signature from"
		"^user: ") nil :next-action 'log :success t)
	     (egg--git-pp-grab-line-no -1 :next-action 'log :success t)))
	((= ret-code 1)
	 (or (egg--git-pp-grab-1st-line-matching '("no signature found" "^error:"))
	     (egg--git-pp-grab-line-no -1)))
	(t ;; 128
	 (egg--git-pp-fatal-result 
	  "gpg: skipped\\|^gpg: \\|empty.+object\\|bad object\\|[Nn]o tag"))))

(defun egg--git-tag-cmd (buffer-to-update stdin &optional args)
  (if stdin
      (egg--do-git-action-stdin "tag" stdin buffer-to-update #'egg--git-tag-cmd-pp args)
    (egg--do-git-action "tag" buffer-to-update #'egg--git-tag-cmd-pp args)))

(defun egg--git-tag-check-name (name &optional force ambiguity-ok)
  (let ((check-name (egg-git-to-string "name-rev" name)))
    (setq check-name (save-match-data (split-string check-name " " t)))
    (cond ((and (equal (nth 0 check-name) "Could")
		(equal (nth 1 check-name) "not"))
	   nil) ;; ok, no collision
	  ((and (equal (nth 0 check-name) name)
		(not (equal (nth 1 check-name) name))
		;; collision with existing tag
		(or force
		    (y-or-n-p (format "a tag %s already exists, force move? " name)))))
	  ((and (equal (nth 0 check-name) name)
		(equal (nth 1 check-name) name)
		;; collison with heads
		(unless ambiguity-ok
		  (error "Refuse to introduce ambiguity: a branch head %s alread exist! bailed out!"
			 name))
		nil)))))

(defun egg--git-pp-commit-output (ret-code)
  (cond ((= ret-code 0)
	 (or (egg--git-pp-grab-line-matching "files? changed" nil
					     :success t :next-action 'status)
	     (egg--git-pp-grab-line-no -1 :success t :next-action 'status)))
	((= ret-code 1)
	 (or (egg--git-pp-grab-1st-line-matching '("^nothing" "^Abort") nil
						 :success t)
	     (egg--git-pp-grab-line-no -1)))
	(t (egg--git-pp-fatal-result (regexp-opt '("empty message" "nothing to amend"))
				     "[Oo]nly one.+ can be used"))))

(defun egg--git-commit-with-region-cmd (buffer-to-update beg end gpg-uid &rest args)
  (egg--do-git-action-stdin "commit"
			    (cons beg end) buffer-to-update
			    #'egg--git-pp-commit-output
			    (append args (cond ((eq gpg-uid t) (list "-v" "-S" "-F" "-"))
					       ((stringp gpg-uid) (list "-v" 
									(concat "--gpg-sign=" gpg-uid)
									"-F" "-"))
					       (t (list "-v" "-F" "-"))))))


(defun egg--async-create-signed-commit-cmd (buffer-to-update beg end gpg-uid &rest extras)
  (let ((args (list "-v" (if (stringp gpg-uid)
			     (concat "--gpg-sign=" gpg-uid)
			   "-S") 
		    "-m" (buffer-substring-no-properties beg end))))
    (setq args (nconc args extras))
    (egg-async-1-args (list #'egg--async-create-signed-commit-handler buffer-to-update)
		      (cons "commit" args))))

(defun egg-cleanup-n-commit-msg (cmd-func beg end gpg-uid &rest extra-commit-options)
  (apply cmd-func t beg end gpg-uid 
	 (cons (if (and (save-excursion (goto-char beg) (re-search-forward "^#" end t))
			(y-or-n-p (format "should lines beginning with # be removed? ")))
		   "--cleanup=strip"
		 "--cleanup=whitespace")
	       extra-commit-options)))

(defun egg--git-amend-no-edit-cmd (buffer-to-update &rest args)
  (egg--do-git-action
   "commit" buffer-to-update #'egg--git-pp-commit-output
   (nconc (list "--amend" "--no-edit") args)))


(defsubst egg-list-stash (&optional ignored)
  (egg--git t "stash" "list"))

(defun egg-run-git-log (ref &optional git-log-extra-options paths)
  (let (max-count-option max-count)
    (setq max-count-option 
	  (cond ((null ref)
		 (and egg-log-all-max-len
		      (list (format "--max-count=%d" egg-log-all-max-len))))
		((and egg-log-HEAD-max-len
		      (consp ref) 
		      (eq (car ref) :locate)
		      (= (length (cdr ref)) 2))
		 (setq max-count (max (egg-git-range-length (nth 1 ref) (nth 2 ref))
				      egg-log-HEAD-max-len))
		 (setq ref (cdr ref))	;; remove :locate
		 (list (format "--max-count=%d" max-count)))
		((and (or (stringp ref) (consp ref)) egg-log-HEAD-max-len)
		 (list (format "--max-count=%d" egg-log-HEAD-max-len)))
		(t (error "Invalid ref: %s" ref))))
    (egg--git-args 
     t (nconc (list "--no-pager" "log"
		    "--pretty=oneline"
		    "--decorate=full"
		    "--no-color")
	      max-count-option
	      git-log-extra-options
	      ;; (when (and paths (null (cdr paths)))
	      ;;   (list "--follow"))
	      (cond ((null ref) (list "--all"))
		    ((stringp ref) (list ref))
		    ((consp ref) ref))
	      (when paths (cons "--" paths))))))

(provide 'egg-git)
