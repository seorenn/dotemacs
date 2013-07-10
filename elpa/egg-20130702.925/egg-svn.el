;;; egg-svn.el --- Emacs Got Git - Emacs interface to Git

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
;;
;; Commentary:
;;
;; This is NOT for typical git-svn usage. This package is a tool
;; to help dealing with HUMONGOUS svn repo via git-svn.
;;
;;

(require 'egg-git)
(require 'egg-const)
(require 'egg)

(defcustom egg-svn-command "svn"
  "Name or full-path to the svn command.
Set this to the appropriate string in the case where `svn' is not the
desirable way to invoke GIT."
  :group 'egg
  :type 'string)


(defcustom egg-svn-profile-alist 
  `(("Standard" 
     :email "myself@y.svn.host"
     :namespace "svn"
     :url "http://my.svn.host/myrepo"
     :exclude "/tags/"
     :trunk ("trunk" . "refs/remotes/svn/trunk")
     :branches (("branches/" . "refs/remotes/svn/"))
     :oldest nil
     )
    ("Large" 
     :email ,user-mail-address
     :namespace "svn"
     :url "http://my.svn.host/large_repo"
     :exclude "/tags/"
     :trunk ("main" . "refs/remotes/svn/main")
     :branches (("branches/release/12.x/" . "refs/remotes/svn/")
		(,(concat "branches/private/" user-login-name "/") .
		 ,(concat "refs/remotes/" user-login-name "/")))
     :oldest "branches/release/12.3.0"
     ))
  "Profiles to create new git-svn repository."
  :group 'egg
  :type '(repeat (list (string :tag "Profile Name")
		       (const :tag "----" :email)
		       (string :tag "Email Address")
		       (const :tag "----" :namespace)
		       (string :tag "Namespace (similar to origin)")
		       (const :tag "----" :url)
		       (string :tag "URL of repo")
		       (const :tag "----" :exclude)
		       (string :tag 
			       "Perl Regexp to ignore paths on fetch (be very careful with this)")
		       (const :tag "----" :trunk)
		       (cons :tag "Trunk mapping"
			     (string :tag "SVN Trunk Name")
			     (string :tag "Git Ref Name"))
		       (const :tag "----" :branches)
		       (repeat :tag "Branch mappings"
			       (cons :tag "Branch"
				     (string :tag "SVN Branch Prefix (including final /)")
				     (string :tag "Git Ref Prefix (including final /)")))
		       
		       (const :tag "----" :oldest)
		       (choice :tag "Limit SVN History Fetching to"
			       (const :tag "Off (Fetch All Revisions)" nil)
			       (integer :tag "SVN Revision")
			       (string :tag "SVN Branch Path")))))

(defsubst egg--svn (buffer &rest args)
  "run SVN with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (= (apply 'call-process egg-svn-command nil buffer nil args) 0))

(defsubst egg--svn-args (buffer args)
  "run SVN with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (= (apply 'call-process egg-svn-command nil buffer nil args) 0))


(defun egg--do-svn (stdin cmd args)
  "Run svn command CMD with ARGS synchronously, using STDIN as starndard input.
ARGS should be a list of arguments for the git command CMD."
  (egg--do stdin egg-svn-command (cons cmd args)))


(defun egg-svn-lines-matching (re idx &rest args)
  "run SVN with ARGS.
Return the output lines as a list of strings."
  (with-temp-buffer
    (when (egg--svn-args t args)
      (let (lines)
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (setq lines (cons (match-string-no-properties idx) lines)))
          lines)))))


(defsubst egg-git-svn-remote-name ()
  (egg-pick-file-contents (concat (egg-git-dir) "/config")
			  "^\\[svn-remote +\"\\(.+\\)\"\\]$" 1))

(defsubst egg-git-svn-config-name (name &optional remote)
  (concat "svn-remote." (or remote (egg-git-svn-remote-name)) "." name))

(defsubst egg-git-svn-url (&optional remote)
  (egg-git-to-string "config" "--get" (egg-git-svn-config-name "url" remote)))

(defsubst egg-git-svn-ignore-paths (&optional remote)
  (egg-git-to-string "config" "--get" (egg-git-svn-config-name "ignore-paths" remote)))

(defsubst egg-svn--s2n (string)
  (and string (stringp string) (string-to-number string)))

(defsubst egg-git-svn-max-rev (&optional remote)
  (egg-svn--s2n (egg-git-to-string "config" "--file" (concat (egg-git-dir) "/svn/.metadata")
				"--get" (egg-git-svn-config-name "branches-maxRev" remote))))

(defsubst egg-git-svn-set-max-rev (svn-rev &optional remote)
  (egg--git nil "config" "--file" (concat (egg-git-dir) "/svn/.metadata")
	    "--replace-all" (egg-git-svn-config-name "branches-maxRev" remote)
	    (if (stringp svn-rev) svn-rev (number-to-string svn-rev))))

(defsubst egg-git-svn-epoch-rev (&optional remote)
  (egg-svn--s2n (car (egg-git-lines-matching "^r\\([0-9]+\\) " 1 "svn" "log"
					     "--reverse" "--limit" "1"))))


(defsubst egg-svn-name-to-full-ref-name (&optional svn-name)
  (let ((default-directory (concat (egg-git-dir) "/svn/")))
    (car (file-expand-wildcards (concat "refs/remotes/*/" svn-name)))))

(defsubst egg-svn-name-to-short-ref-name (&optional svn-name)
  (let ((default-directory (concat (egg-git-dir) "/svn/refs/remotes/")))
    (car (file-expand-wildcards (concat "*/" svn-name)))))

(defsubst egg-svn-is-known-ref (full-ref &optional svn-name)
  (file-directory-p (concat (egg-git-dir) "/svn/" full-ref)))

(defsubst egg-svn-all-refs (&optional prefix)
  (let ((default-directory (concat (egg-git-dir) "/svn/refs/remotes/")))
    (file-expand-wildcards (concat (or prefix "*") "/*"))))

(defsubst egg-svn-all-full-refs (&optional prefix)
  (let ((default-directory (concat (egg-git-dir) "/svn/")))
    (file-expand-wildcards (concat "refs/remotes/" (or prefix "*") "/*"))))

(defun egg-svn-get-all-prefixes ()
  (delete-dups (egg-git-lines-matching "^svn-remote.+:refs/remotes/\\([^/]+\\)/.+$" 1 
				       "config" "--get-regexp"
				       "svn-remote\\..+\\\.(branches|fetch)")))

(defsubst egg-svn-path-exists-p (path-url)
  (egg--svn nil "info" path-url))

(defsubst egg-svn-path-last-rev (path-url)
  (egg-svn--s2n (car (egg-svn-lines-matching "^Last Changed Rev: \\([0-9]+\\)$" 1
					     "info" path-url))))

(defsubst egg-svn-path-first-rev (path-url)
  (egg-svn--s2n (car (egg-svn-lines-matching "^r\\([0-9]+\\) " 1 "log" "--stop-on-copy" 
					     "-r" "0:HEAD" "-l1" path-url))))

(defsubst egg-git-svn-ref-last-rev (full-ref)
  (egg-svn--s2n (egg-git-to-string "svn" "find-rev" full-ref)))


(defun egg-svn-decode-rev-map (info)
  (with-temp-buffer
    (let ((name (plist-get info :name))
	  (file-name (plist-get info :file))
	  (record-no 0)
	  mappings record svn-rev git-sha1 max-rev min-rev map-vec) 
      (insert-file-contents-literally file-name)
    (goto-char (point-min))
      (while (not (eobp))
	(setq record (decode-coding-string 
		      (buffer-substring-no-properties (point) (+ (point) 24))
		      'raw-text-unix))
	(setq git-sha1 (substring record 4))
	(setq svn-rev (logior (lsh (aref record 0) 24)
			      (lsh (aref record 1) 16)
			      (lsh (aref record 2) 8)
			      (lsh (aref record 3) 0)))
	(setq git-sha1 (mapconcat (lambda (val) (format "%02x" val)) git-sha1 ""))
	(push (list svn-rev git-sha1 record-no) mappings)
	(forward-char 24)
	(setq record-no (1+ record-no)))
      (when mappings
	(setq max-rev (car (car mappings)))
	(setq mappings (nreverse mappings))
	(setq min-rev (car (car mappings)))
	(setq map-vec (make-vector (- max-rev min-rev -1) nil))
	(mapc (lambda (item)
		(aset map-vec (- (car item) min-rev) item))
	      mappings)
	(list name :file file-name :min min-rev :max max-rev :map map-vec)))))

(defun egg-svn-get-ref-rev-map (full-ref)
  (cdr (egg-svn-decode-rev-map 
	(list :file (car (file-expand-wildcards (concat (egg-git-dir) "/svn/"
							full-ref "/.rev_map.*")))
	      :name full-ref))))

(defun egg-svn-reset-ref-to-rev (full-ref svn-rev)
  (let* ((info (egg-svn-get-ref-rev-map full-ref))
	 (map (plist-get info :map))
	 (min (plist-get info :min))
	 (max (plist-get info :max))
	 (file (plist-get info :file))
	 (old-sha (egg-git-to-string "rev-parse" full-ref))
	 vec-idx record-no rev-info record read-rev new-sha del-len)
    (when (and old-sha
	       (file-readable-p file) (file-writable-p file)
	       (>= svn-rev min))
      (if (> svn-rev max)
	  0
	(setq vec-idx (- svn-rev min))
	(setq rev-info (aref map vec-idx))
	(when (and (consp rev-info) (numberp (car rev-info)) (= (nth 0 rev-info) svn-rev))
	  (setq record-no (nth 2 rev-info))
	  (setq new-sha (nth 1 rev-info))
	  (with-temp-buffer
	    (set-buffer-file-coding-system 'raw-text-unix)
	    (insert-file-contents-literally file)
	    (goto-char (point-min))
	    (forward-char (* record-no 24))
	    (setq record (decode-coding-string 
			  (buffer-substring-no-properties (point) (+ (point) 24))
			  'raw-text-unix))
	    (unless (= (setq read-rev (logior (lsh (aref record 0) 24)
					      (lsh (aref record 1) 16)
					      (lsh (aref record 2) 8)
					      (lsh (aref record 3) 0)))
		       svn-rev)
	      (error "Internal error: record %d of %s has rev=%4x, expected rev=%4x"
		     record-no file read-rev svn-rev))
	    (forward-char 24)
	    (setq del-len (- (point-max) (point)))
	    (when (> del-len 0)
	      (unless (egg--git nil "update-ref" full-ref new-sha old-sha)
		(error "Error: failed to move %s back to %s from %s"
		       full-ref new-sha old-sha))
	      (delete-region (point) (point-max))
	      (write-region (point-min) (point-max) file)
	      (/ del-len 24))))))))


(defun egg--do-svn-action (cmd buffer-to-update post-proc-func args)
  "Run svn command CMD with arguments list ARGS.
Show the output of CMD as feedback of the emacs command.
Update the buffer BUFFER-TO-UPDATE and use POST-PROC-FUNC as the
output processing function for `egg--do-handle-exit'."
  (egg--do-show-output (concat "SVN-" (upcase cmd))
		       (egg--do-handle-exit (egg--do-svn nil cmd args)
					    post-proc-func buffer-to-update)))

(defun egg--git-svn-init-repo (buffer-to-update svn-remote svn-trunk-ref svn-url
						ignore-paths-pcre)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0) (egg--git-pp-grab-line-matching "Initialized empty Git repository"
							   :success t :next-action 'status))
	   (t (egg--git-pp-fatal-result))))
   (nconc (list "init" "-R" svn-remote "-i" svn-trunk-ref svn-url)
	  (if ignore-paths-pcre
	      (list (concat "--ignore-paths=" ignore-paths-pcre))))
   'no-log))

(defun egg--git-svn-create-branch (buffer-to-update log-msg branch-name 
						    &optional svn-parent-url)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (or (egg--git-pp-grab-line-matching (concat "^r[0-9]+ = .+ (refs/.+/"
							branch-name ")$")
						:success t :next-action 'log)
		(egg--git-pp-grab-line-matching "^r[0-9]+ = .+ (refs/.+)$"
						:success t :next-action 'log)
		(egg--git-pp-grab-line-matching "^Successfully.+" :success t
						:next-action 'log)
		(egg--git-pp-grab-line-matching "^Found branch.+" :success t
						:next-action 'log)
		(egg--git-pp-grab-line-matching "^Found possible branch.+" :success t
						:next-action 'log)
		(egg--git-pp-grab-line-matching (concat "/" branch-name) :success t
						:next-action 'log)))
	   (t (egg--git-pp-fatal-result))))
   (nconc (list "branch" "-m" log-msg)
	  (if svn-parent-url (list "-d" svn-parent-url))
	  (list branch-name))))

(defun egg--git-svn-reset-max-rev (buffer-to-update svn-rev)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (or (egg--git-pp-grab-line-matching "^r[0-9]+ =" :success t)
		(egg--git-pp-grab-line-no -1 :success t)))
	   (t (egg--git-pp-fatal-result))))
   (list "reset" (concat "-r" (if (stringp svn-rev) svn-rev (number-to-string svn-rev))))))

(defun egg--git-svn-fetch-rev (buffer-to-update svn-name svn-rev)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (or (egg--git-pp-grab-line-matching "^r[0-9]+ =" :success t :next-action 'log)
		(egg--git-pp-grab-line-no -1 :success t :next-action 'log)))
	   (t (egg--git-pp-fatal-result))))
   (list "fetch" (concat "-r" (if (stringp svn-rev) svn-rev (number-to-string svn-rev)))
	 svn-name)))

(defun egg--git-svn-fetch (buffer-to-update svn-name)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (or (egg--git-pp-grab-line-matching-backward "^r[0-9]+ =" :success t
							 :next-action 'log)
		(egg--git-pp-grab-line-no -1 :success t :next-action 'log)))
	   (t (egg--git-pp-fatal-result))))
   (list "fetch" svn-name)))

(defun egg--git-svn-dcommit (buffer-to-update branch)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((memq ret-code '(0 1))
	    (or (egg--git-pp-grab-line-matching "^r[0-9]+ =" :success t :next-action 'log)
		(egg--git-pp-grab-line-matching "^dcommitted " :success t :next-action 'log)
		(egg--git-pp-grab-line-matching "^Committed " :success t :next-action 'log)
		(egg--git-pp-grab-line-no -1 :success t :next-action 'log)))
	   (t (egg--git-pp-fatal-result))))
   (list "dcommit" branch)))

(defun egg--svn-delete (buffer-to-update log-msg svn-url)
  (egg--do-svn-action
   "delete" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (egg--git-pp-grab-line-matching "^Committed revision.+" :success t
					    :next-action 'log))
	   (t (egg--git-pp-fatal-result))))
   (list "-m" log-msg svn-url)))

(defun egg--svn-copy (buffer-to-update log-msg from-url to-url)
  (egg--do-svn-action
   "copy" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (egg--git-pp-grab-line-matching "^Committed revision.+" :success t
					    :next-action 'log))
	   (t (egg--git-pp-fatal-result))))
   (list "-m" log-msg from-url to-url)))

(defun egg--svn-get-parent-revision (svn-branch-path &optional remote-name)
  (with-egg-debug-buffer
    (let* ((url (egg-git-svn-url remote-name))
	  (path (and url (concat url "/" svn-branch-path))))
      (erase-buffer)
      (when (egg--svn-args t (list "log" "-v" "--stop-on-copy" 
				   "-r" "0:HEAD" "--limit" "1" path))
	(goto-char (point-min))
	(when (re-search-forward (concat svn-branch-path ".* (from /.+:\\([0-9]+\\))$") nil t)
	  (string-to-number (match-string-no-properties 1)))))))

(defun egg--svn-get-birth-info (svn-branch-path &optional remote-name)
  (with-egg-debug-buffer
    (let* ((url (egg-git-svn-url remote-name))
	  (path (and url (concat url "/" svn-branch-path))))
      (erase-buffer)
      (when (egg--svn-args t (list "log" "-v" "--stop-on-copy" 
				   "-r" "0:HEAD" "--limit" "1" path))
	(goto-char (point-min))
	(when (re-search-forward (concat svn-branch-path ".* (from /\\(.+\\):\\([0-9]+\\))$")
				 nil t)
	  (cons (match-string-no-properties 1)
		(string-to-number (match-string-no-properties 2))))))))

(defun egg-svn-start-initial-fetch (process msg)
  (let ((profile (if (processp process) (process-plist process) process))
	(process (if (processp process) nil))
	(buffer (if (processp process) (process-buffer process) (current-buffer)))
	todo)

    (setq todo (plist-get profile :todo))
    (setq todo (prog1 (car todo)
		 (plist-put profile :todo (cdr todo))))

    (when (stringp msg)
      (goto-char (point-max))
      (insert "GIT-SVN:" msg "\n"))

    (if todo
	(with-current-buffer buffer
	  (setq process (funcall todo profile))
	  (if (processp process)
	      (progn
		(set-process-plist process profile)
		(set-process-sentinel process #'egg-svn-start-initial-fetch))
	    (message "cannot start svn to do initial fetching!")))
      (goto-char (point-max))
      (insert "EGG-SVN:done fetching initial SVN revisions.\n")
      (egg-log nil))))

(defun egg-svn-init-repo (profile)
  (interactive (let ((alist egg-svn-profile-alist)
		     name)
		 (setq name (completing-read "init an git-svn repo based on profile: " 
					     alist nil t))
		 (list (assoc name egg-svn-profile-alist))))
  (let* ((dir default-directory)
	 (buffer (get-buffer-create (concat "*git-svn-init@" dir "*")))
	 (profile-name (car profile))
	 (profile (copy-sequence (cdr profile)))
	 (remote (plist-get profile :namespace))
	 (trunk-full-ref (cdr (plist-get profile :trunk)))
	 (oldest (plist-get profile :oldest))
	 (ignore-re (plist-get profile :exclude))
	 (trunk (car (plist-get profile :trunk)))
	 (branch-mappings (plist-get profile :branches))
	 trunk-ref 
	 first-fetch-func last-fetch-func todo
	 tmp)
    (with-current-buffer buffer
      (erase-buffer)
      (pop-to-buffer buffer)

      (setq trunk-ref 
	    (mapconcat 'identity 
		       (nthcdr 2 (save-match-data (split-string trunk-full-ref "/" t))) "/"))
      
      (if (setq tmp (egg--git-svn-init-repo nil remote trunk-ref 
					    (plist-get profile :url) 
					    (plist-get profile :exclude)))
	  (insert (or (plist-get tmp :line) "init has not output") "\n")
	(error "Failed to init git-svn repo in %s" dir))

      (when (or (/= (call-process egg-git-command nil t t "config" "--replace-all"
				   "user.email" (plist-get profile :email)) 0)
		(/= (call-process egg-git-command nil t t "config" "--replace-all"
				  (concat "svn-remote." remote ".fetch")
				  (concat trunk ":" trunk-full-ref)) 0)
		(memq nil (mapcar (lambda (svn-ref)
				    (= (call-process egg-git-command nil t t "config" "--add"
						     (concat "svn-remote." remote ".branches")
						     (concat (car svn-ref) "*:" 
							     (cdr svn-ref) "*")) 0))
				  branch-mappings)))
	(error "Failed to configure git-svn repo in %s" dir))


      (setq oldest (cond ((numberp oldest) oldest)
			 ((stringp oldest)
			  (egg--svn-get-parent-revision oldest remote))
			 (t nil)))
      (when oldest 
	(setq oldest (number-to-string oldest))
	(plist-put profile :oldest oldest))
      
      (when (y-or-n-p (format "proceed to fetch %ssvn revisions%s (this might take %s)? "
			      (if oldest "" "all ")
			      (if oldest (format " from r%s to HEAD" oldest) "")
			      (propertize "weeks" 'face 'bold)))

	(setq last-fetch-func
		(lambda (profile)
		  (goto-char (point-max))
		  (insert "EGG-SVN: fetching initial revisions:\n")
		  (insert "GIV-SVN: fetch all\n")
		  (start-process "egg-svn-fetch" (current-buffer) egg-git-command
				 "svn" "-q" "fetch")))
	(push last-fetch-func todo)

	(when oldest
	  (setq first-fetch-func
		(lambda (profile)
		  (goto-char (point-max))
		  (insert "EGG-SVN: fetching 1st revision:\n")
		  (insert "GIV-SVN: fetch r" (plist-get profile :oldest) "\n")
		  (start-process "egg-svn-fetch" (current-buffer) egg-git-command "svn" "-q"
				 "fetch" "-r" (plist-get profile :oldest))))
	  (push first-fetch-func todo))

	(plist-put profile :todo todo)

	(egg-svn-start-initial-fetch profile nil)))))

(defsubst egg-svn-full-to-remote (full-ref)
  (file-name-nondirectory (directory-file-name (file-name-directory full-ref))))

(defun egg-svn-map-name (name branch spec)
  (save-match-data
    (let ((mappings (mapcar (lambda (line)
			      (split-string line "\\(?:{[a-zA-Z0-9,_-]+}\\)?[:* \t]+" t))
			    (egg-git-to-lines "config" "--get-regexp" 
					      "svn-remote\\..*\\.(branches|fetch)")))
	  (full-branch (cond ((and branch (string-match "\\`refs/remotes/" branch))
			      branch)
			     ((and spec branch)
			      (concat spec branch))))
	  match tmp remote type props)
      (setq mappings
	    (mapcar 
	     (lambda (map)
	       (setq tmp (split-string (car map) "\\." t))
	       (setq remote (nth 1 tmp))
	       (setq type (nth 2 tmp))
	       (list (nth 2 map) remote (intern (concat ":" type)) (nth 1 map) (nth 2 map)))
	     mappings))
      (setq match (or (and full-branch (assoc full-branch mappings))
		      (and branch (assoc branch mappings))
		      (and spec (assoc spec mappings))))
      (when match
	(setq props (list :x-delete #'egg-delete-svn-path
			  :x-push #'egg-push-to-svn
			  :x-fetch #'egg-fetch-from-svn
			  :x-info (cdr match)))
	(if (stringp name)
	    (apply #'propertize name props)
	  props)))))

(defun egg-svn-add-remote-properies (name prefix &optional branch)
  (egg-svn-map-name name branch (concat "refs/remotes/" prefix "/")))

(defun egg-svn-get-remote-properies (prefix branch)
  (or (and (not (equal prefix "."))
	   (egg-svn-map-name nil branch (concat "refs/remotes/" prefix "/")))
      (and (stringp branch) 
	   (egg-svn-map-name nil branch (file-name-directory branch)))))

(defun egg-svn-ref-to-path (r-ref x-info)
  (when (and (stringp r-ref) (consp x-info))
    (let ((r-ref (file-name-nondirectory r-ref))
	  (svn-name (car x-info))
	  (map-type (nth 1 x-info))
	  (dest (nth 2 x-info))
	  url)
      (setq url (and svn-name (egg-git-svn-url svn-name)))
      (when (and (stringp url) (memq map-type '(:fetch :branches)) (stringp dest))
	(concat url "/" (cond ((eq map-type :fetch) dest)
			      ((eq map-type :branches) (concat dest r-ref))))))))

(defun egg-svn-ref-to-git-svn-dir (r-ref x-info)
  (when (and (stringp r-ref) (consp x-info))
    (let ((r-ref (file-name-nondirectory r-ref))
	  (local-base (nth 3 x-info)))
      (when (stringp local-base)
	(concat (egg-git-dir) "/svn/" local-base r-ref)))))

(defun egg-delete-svn-path (buffer-to-update x-info r-ref)
  (let ((svn-path (egg-svn-ref-to-path r-ref x-info))
	(local-dir (egg-svn-ref-to-git-svn-dir r-ref x-info))
	(r-ref (file-name-nondirectory r-ref))
	res)
    (when (stringp svn-path)
      (setq res (if (or (save-match-data (string-match "@" r-ref))
			(not (egg-svn-path-exists-p svn-path))) 
		    ;; git-svn's metadata only, not on svn repo
		    (list :success t :next-action 'log)
		  (egg--svn-delete buffer-to-update (concat "delete " r-ref)
				   svn-path)))
      (when (and (plist-get res :success) 
		 (stringp local-dir)
		 (file-directory-p local-dir))
	(delete-directory local-dir t)))
    res))

(defun egg-svn-find-last-mapped-rev (git-start)
  (let ((commit (egg-git-to-string "rev-parse" (concat git-start "^{/git-svn-id:}")))
	found svn-rev)
    (while (and (not found) commit)
      (setq svn-rev (egg-git-to-string "svn" "find-rev" commit))
      (unless (and svn-rev
		   (setq found (egg-git-to-string "svn" "find-rev" (concat "r" svn-rev)
						  git-start))
		   (equal found commit))
	(setq found nil)
	(setq commit (egg-git-to-string "rev-parse" (concat commit "^^{/git-svn-id:}")))))
    found))

(defun egg-git-svn-map-svn-path (path)
  (save-match-data
    (let* ((path (directory-file-name path))
	   (path-prefix (file-name-directory path))
	   (name (file-name-nondirectory path))
	   (svn-name (egg-git-svn-remote-name))
	   (direct (mapcar (lambda (line)
			     (split-string line ":" t))
			   (egg-git-to-lines "config" "--get-all" 
					     (egg-git-svn-config-name "fetch" svn-name))))
	   (branches (mapcar (lambda (line)
			       (split-string line "[*:]+" t))
			     (egg-git-to-lines "config" "--get-all" 
					       (egg-git-svn-config-name "branches" 
									svn-name))))
	   match)
      (cond ((setq match (assoc path direct))
	     (nth 1 match))
	    ((setq match (assoc path-prefix branches))
	     (concat (nth 1 match) name))
	    (t nil)))))

(defun egg-git-svn-map-full-ref (full-ref)
  (let* ((short-ref (file-name-nondirectory full-ref))
	 (local-base (file-name-directory full-ref))
	 (svn-name (egg-git-svn-remote-name))
	 (direct (mapcar (lambda (line)
			   (nreverse (split-string line ":" t)))
			 (egg-git-to-lines "config" "--get-all" 
					   (egg-git-svn-config-name "fetch" svn-name))))
	 (branches (mapcar (lambda (line)
			     (nreverse (split-string line 
						     "\\(?:{[a-z0-9A-Z_,-]+}\\)?[*:]+" t)))
			   (egg-git-to-lines "config" "--get-all" 
					     (egg-git-svn-config-name "branches" svn-name))))
	 match)
    (cond ((setq match (assoc full-ref direct))
	   (nth 1 match))
	  ((setq match (assoc local-base branches))
	   (concat (nth 1 match) short-ref))
	  (t nil))))


(defun egg--git-svn-add-custom-mapping (type mapping)
  (let* ((key (egg-git-svn-config-name type))
	 (existings (egg-git-to-string-list "config" "--get-all" key)))
    (unless (assoc mapping existings)
      (egg--git nil "config" "--add"  key mapping)
      mapping)))

(defun egg--git-svn-rm-custom-mapping (type mapping)
  (let ((key (egg-git-svn-config-name type))
	(value (regexp-quote mapping)))
    (egg--git nil "config" "--unset"  key mapping)))

(defsubst egg--git-svn-add-custom-branch-mapping (mapping)
  (egg--git-svn-add-custom-mapping "branches" mapping))

(defsubst egg--git-svn-add-custom-direct-mapping (mapping)
  (egg--git-svn-add-custom-mapping "fetch" mapping))

(defsubst egg--git-svn-rm-custom-branch-mapping (mapping)
  (egg--git-svn-rm-custom-mapping "branches" mapping))

(defsubst egg--git-svn-rm-custom-direct-mapping (mapping)
  (egg--git-svn-rm-custom-mapping "fetch" mapping))

(defun egg--git-svn-add-direct-mapping (svn-path git-full-ref)
  (egg--git-svn-add-custom-direct-mapping (concat svn-path ":" git-full-ref)))

(defmacro with-egg-temp-direct-mapping (mapping &rest body)
  "Setup a temoporary svn-remote.svn.fetch mapping. Evaluate BODY there like `progn'."
  (declare (indent 1) (debug t))
  (let ((egg-added-mapping (make-symbol "egg-added-mapping"))
	(egg-mapping (make-symbol "egg-mapping"))
	(egg-result (make-symbol "egg-result")))
    `(let* ((,egg-mapping mapping)
	    (,egg-added-mapping (egg--git-svn-add-custom-direct-mapping ,egg-mapping))
	    ,egg-result)
       (unwind-protect
	   (setq ,egg-result (progn ,@body))
	 (when ,egg-added-mapping
	   (egg--git-svn-rm-custom-direct-mapping ,egg-mapping)))
       ,egg-result)))

(defun egg-git-svn-do-fetch (buffer-to-update svn-name svn-path full-ref &optional svn-rev)
  (let ((mapping (concat svn-path ":" full-ref)))
    (with-egg-temp-direct-mapping mapping
      (if svn-rev
	  (egg--git-svn-fetch-rev buffer-to-update svn-name svn-rev)
	(egg--git-svn-fetch buffer-to-update svn-name)))))

(defun egg-git-svn-do-refetch (buffer-to-update svn-name svn-path full-ref svn-rev)
  (when (egg-svn-reset-ref-to-rev full-ref svn-rev)
    (egg-git-svn-set-max-rev svn-rev svn-name)
    (egg-git-svn-do-fetch buffer-to-update svn-name svn-path full-ref)))

(defun egg-svn-make-branch-from (buffer-to-update svn-repo-name new-url from-url
						  &optional expected-git-ref)
  (let ((res (egg--svn-copy nil (concat "create branch " (file-name-nondirectory new-url))
			    from-url new-url))
	(pretty-new (propertize new-url 'face 'bold))
	(pretty-from (propertize from-url 'face 'bold))
	line ok new-rev fetched-rev)
    
    (setq line (plist-get res :line))
    (if (not (plist-get res :success))
	(error "Failed to create %s from %s: %s" pretty-new pretty-from line)
      (setq new-rev (save-match-data
		      (if (string-match "Committed revision \\([0-9]+\\)\\." line)
			  (match-string-no-properties 1 line)
			(error "Can't parse svn revision number in: \"%s\"" line))))
      (setq new-rev (string-to-number new-rev))
      (setq res (egg--git-svn-fetch buffer-to-update svn-repo-name))
      (setq line (plist-get res :line))
      (if (not (plist-get res :success))
	  (error "Failed to do post-copy fetch: %s" line)
	(setq fetched-rev (egg-git-svn-max-rev svn-repo-name))
	(if (>= fetched-rev new-rev)
	    (setq ok t)
	  (setq res (egg--git-svn-fetch-rev buffer-to-update svn-repo-name new-rev))
	  (setq line (plist-get res :line))
	  (if (not (plist-get res :success))
	      (error "Failed to fetch svn revision %s: %s" new-rev line)
	    (setq fetched-rev (egg-git-svn-max-rev svn-repo-name))
	    (if (>= fetched-rev new-rev)
		(setq ok (or (null expected-git-ref) 
			     (egg--git nil "show-ref" expected-git-ref)))
	      (error "Problems with git-svn: needs to fetch r%s but git-svn only fetched up to r%s" 
		     new-rev fetched-rev))))))
    ok))


(defun egg-push-to-svn (buffer-to-update svn-remote l-ref r-ref)
  (let ((svn-name (nth 0 svn-remote))
	(svn-prefix (nth 2 svn-remote))
	(local-base (nth 3 svn-remote))
	vremote base r-name)
    (save-match-data
      (cond ((and (string-match "\\`refs/remotes/\\([^/]+\\)/\\(.+\\)\\'" r-ref)
		  (stringp local-base)
		  (equal (file-name-directory r-ref) local-base))
	     ;; already-mapped svn branch
	     (setq vremote (match-string-no-properties 1 r-ref))
	     (setq r-name (match-string-no-properties 2 r-ref))
	     (message "GIT-SVN> prepare to push %s to %s on %s..." l-ref r-name vremote)
	     (egg-do-push-to-svn buffer-to-update svn-remote l-ref r-ref))
	    ((and (not (string-match "/" r-ref)) svn-prefix)
	     ;; possibly new branch under existing prefix
	     (message "GIT-SVN> prepare to push %s to %s on %s..." 
		      l-ref (concat svn-prefix r-ref) svn-name)
	     (egg-do-push-to-svn buffer-to-update svn-remote l-ref r-ref))
	    ((and (string-match "\\`\\(.+\\)/\\([^/]+\\)\\'" r-ref) svn-remote)
	     ;; pushing new branch into a new prefix of an existing svn remote
	     (message "GIT-SVN> prepare to push %s to %s on %s..."
		      l-ref r-ref svn-name)
	     (egg-push-to-new-svn-prefix buffer-to-update svn-remote l-ref
				    (match-string-no-properties 1 r-ref)
				    (match-string-no-properties 2 r-ref)))
	    (t
	     (error "Don't know where to push %s->%s" l-ref r-ref))))))

(defun egg-push-to-new-svn-prefix (buffer-to-update svn-remote l-ref prefix r-name)
  (let* ((svn-name (car svn-remote))
	 (url (and svn-name (egg-git-svn-url svn-name)))
	 (dest-url (and url (concat url "/" prefix)))
	 (svn-path (concat prefix "/" r-name))
	 (pretty-dest-url (and dest-url (propertize dest-url 'face 'bold)))
	 (pretty-svn (and svn-name (propertize svn-name 'face 'bold)))
	 (pretty-svn-path (and svn-path (propertize svn-path 'face 'bold)))
	 full-ref local-base mapping)
    (when (catch 'new-svn-prefix
	    (unless (egg-svn-path-exists-p dest-url)
	      (unless (y-or-n-p (format "create new svn dir %s? " pretty-dest-url))
		(throw 'new-svn-prefix nil))
	      (egg--svn nil "mkdir" dest-url))

	    (setq full-ref 
		  (or (setq mapping (egg-git-svn-map-svn-path svn-path))
		      (read-string (format "map %s to: " pretty-svn-path) 
				   (concat "refs/remotes/" (file-name-nondirectory prefix)
					   "/" r-name))))

	    (unless (and (stringp full-ref) (string-match "\\`refs/remotes/" full-ref))
	      (message "Cannot handle remote trackign branch name: %s" full-ref)
	      (throw 'full-ref nil))

	    (setq local-base (file-name-directory full-ref))

	    (unless mapping
	      (setq mapping (concat prefix "/{" r-name "}:" local-base "*"))
	      (setq mapping 
		    (or (and (y-or-n-p (concat "add svn->git mapping rule: \""
					       mapping "\" ? "))
			     mapping)
			(read-string "add svn->git mapping rule: " 
				     (concat prefix "/*:" local-base "*"))))
	      (if (string-match (concat ":" local-base "\\*\\'") mapping)
		  (egg--git-svn-add-custom-branch-mapping mapping)
		(message "Cannot handle mapping svn->git mapping: %s" mapping)
		(throw 'full-ref nil)))

	    full-ref)
      (egg-do-push-to-svn buffer-to-update
			  (list svn-name :branches (concat prefix "/")
				local-base)
			  l-ref r-name))))

(defun egg-do-push-to-svn (buffer-to-update svn-remote l-ref r-ref)
  (let* ((svn-name (car svn-remote))
	 (map-type (nth 1 svn-remote))
	 (dest (nth 2 svn-remote))
	 (local-base (cond ((eq map-type :fetch) (file-name-directory (nth 3 svn-remote)))
			   (t (nth 3 svn-remote))))
	 (r-ref (and (stringp r-ref) (file-name-nondirectory r-ref)))
	 (r-full-name (and r-ref (cond ((eq map-type :fetch) (nth 3 svn-remote))
				       (t (concat local-base r-ref)))))
	 (url (and svn-name (egg-git-svn-url svn-name)))
	 (svn-branch-url (and r-ref url 
			      (concat url "/" 
				      (cond ((eq map-type :fetch) dest)
					    ((eq map-type :branches) 
					     (concat dest r-ref))
					    (t (error "Unknown svn-to-git mapping type: %s"
						      map-type))))))
	 (l-is-ref (and (stringp l-ref) (egg-git-to-string "show-ref" l-ref)))
	 (pretty-svn (and svn-name (propertize svn-name 'face 'bold)))
	 (pretty-l-ref (and l-ref (propertize l-ref 'face 'bold)))
	 (pretty-r-full-name (and r-full-name (propertize r-full-name 'face 'bold)))
	 (pretty-svn-url (and svn-branch-url (propertize svn-branch-url 'face 'bold)))
	 (mapping (concat dest "{" r-ref "}:" local-base "*"))
	 git-commit svn-rev res line base-commit)

    (catch 'push-to-svn

      (unless svn-branch-url
	(message "Failed to map branch %s on svn remote %s" r-ref svn-name)
	(throw 'push-to-svn nil))

      (unless (stringp l-ref)
	(message "Can't push local ref: %s" l-ref)
	(throw 'push-to-svn nil))

      (unless (egg-svn-path-exists-p svn-branch-url)
	(setq git-commit (egg-svn-find-last-mapped-rev l-ref))
	(setq svn-rev (and git-commit 
			   (egg-pick-from-commit-message git-commit
							 "^git-svn-id: \\(.+\\) .+$" 1)))
	(unless (y-or-n-p (format "create new svn branch %s at %s (%s)? " 
				  pretty-svn-url (propertize svn-rev 'face 'bold) git-commit))
	  (message "Bailed out before creating new svn branch: %s" pretty-svn-url)
	  (throw 'push-to-svn nil))

	(setq mapping (or (and (y-or-n-p (concat "add svn->git mapping rule: \"" 
						 mapping "\" ? "))
			       mapping)
			  (read-string "add svn->git mapping rule: " 
				       (concat dest "*:" local-base "*"))))

	(if (string-match (concat ":" local-base "\\*\\'") mapping)
	    (egg--git-svn-add-custom-branch-mapping mapping)
	  (message "Cannot handle mapping svn->git mapping: %s" mapping)
	  (throw 'push-to-svn nil))

	(when (egg-svn-make-branch-from buffer-to-update svn-name svn-branch-url svn-rev)
	  (unless (equal (file-name-nondirectory l-ref) (egg-current-branch))
	    (message (if l-is-ref 
			 "created svn branch %s, please rebase %s on %s and push again"
		       "new svn branch %s (from %s) -> %s") 
		     pretty-svn-url pretty-l-ref pretty-r-full-name)
	    (throw 'push-to-svn t))
	  (unless (y-or-n-p (format "rebase %s on top of %s before pushing to %s? "
				    pretty-l-ref pretty-r-full-name pretty-svn))
	    (message "created svn branch %s, please rebase %s on %s and push again" 
		     pretty-svn-url pretty-l-ref pretty-r-full-name)
	    (throw 'push-to-svn t))
	  (unless (egg-do-rebase-head r-full-name nil 'log)
	    (message "trouble rebasing %s on top of %s! bailed out." 
		     pretty-l-ref pretty-r-full-name)
	    (throw 'push-to-svn nil))))

      (setq git-commit (egg-git-to-string "rev-parse" r-full-name))
      (setq base-commit (egg-git-to-string "merge-base" l-ref r-full-name))
      (unless (equal git-commit base-commit)
	(message (if l-is-ref 
		     "please rebase %s on %s before pushing on svn-remote %s"
		   "%s -> %s is not an fast-forward push!")
		 pretty-l-ref pretty-r-full-name pretty-svn)
	(throw 'push-to-svn t))

      (unless (equal (setq res (car (egg-git-lines-matching
				     "Committing to \\(.+\\) \\.\\.\\." 1
				     "svn" "dcommit" "-n" l-ref)))
		     svn-branch-url)
	(message "Fatal: git-svn would dcommit %s on %s instead of %s!"
		 pretty-l-ref (propertize res 'face 'bold) pretty-svn-url)
	(throw 'push-to-svn nil))
      
      (when (y-or-n-p (format "push %s, %d revision(s), on svn-path %s? "
			      pretty-l-ref  
			     (length (egg-git-to-lines "rev-list" 
						       (concat r-full-name ".." l-ref)))
			     pretty-svn-url))
	(egg--git-svn-dcommit buffer-to-update l-ref)))))

(defun egg-fetch-unknown-svn-path (buffer-to-update svn-name svn-path &optional local-base)
  (let* ((svn-path (directory-file-name svn-path))
	 (url (and svn-name (egg-git-svn-url svn-name)))
	 (svn-path-url (and url (concat url "/" svn-path)))
	 (svn-short-name (file-name-nondirectory (directory-file-name svn-path)))
	 (max-rev (and svn-name (egg-git-svn-max-rev svn-name)))
	 (epoch-rev (and svn-name (egg-git-svn-epoch-rev svn-name)))
	 full-ref mapping-config parent-rev branch-first-rev branch-last-rev
	 res line new-rev sha1 birth)
    (save-match-data
      (catch 'full-ref

	(setq full-ref (read-string (format "map %s to: " (propertize svn-path 'face 'bold))
				    (cond ((not (string-match "/" svn-path))
					   (concat "refs/remotes/" svn-name
						   "/" svn-short-name))
					  ((string-match "release" svn-path)
					   (concat "refs/remotes/" svn-name
						   "/" svn-short-name))
					  ((stringp local-base) 
					   (concat local-base svn-short-name))
					  (t nil))))
	(unless (and (stringp full-ref)
		     (string-match "\\`refs/remotes/" full-ref))
	
	  (message "Cannot handle remote trackign branch name: %s" full-ref)
	  (throw 'full-ref nil))
      
	(setq mapping-config (concat (file-name-directory svn-path) 
				     "{" (file-name-nondirectory svn-path) "}:"
				     (file-name-directory full-ref) "*"))
	(setq mapping-config
	      (or (and (y-or-n-p (concat "add svn->git mapping rule: \"" 
					 mapping-config "\" ? "))
		       mapping-config)
		  (read-string "add svn->git mapping rule: " 
			       (concat (file-name-directory svn-path) "*:"
				       (file-name-directory full-ref) "*"))))

	(if (string-match (concat ":" (file-name-directory full-ref) "\\*\\'")
			  mapping-config)
	    (egg--git-svn-add-custom-branch-mapping mapping-config)
	  (message "Cannot handle mapping svn->git mapping: %s" mapping-config)
	  (throw 'full-ref nil))

	(unless (egg-svn-path-exists-p svn-path-url)
	  (message "Failed to find svn path: %s" svn-path-url)
	  (throw 'full-ref nil))


	(setq birth (egg--svn-get-birth-info svn-path svn-name))
	(setq parent-rev (cdr birth))
	(setq branch-last-rev (egg-svn-path-last-rev svn-path-url))
	(setq branch-first-rev (egg-svn-path-first-rev svn-path-url))


	(when (< parent-rev epoch-rev)
	  (message "Cannot fetch revision (r%d) older than epoch (r%d)!" parent-rev epoch-rev)
	  (throw 'full-ref nil))

	(when (< branch-first-rev max-rev)
	  ;; git-svn has already seen branch-first-rev but ignored it!
	  ;; refetch!
	  (if (y-or-n-p (format "unfetch and refetch %d revisions (might take a long time)? "
				(- max-rev parent-rev 1)))
	      ;; re-fetch old revisions to catch the branch
	      (progn
		(message "unfetch and refetch from r%d..." parent-rev)
		(setq res (egg-git-svn-do-refetch buffer-to-update  svn-name
						  (car birth)
						  (egg-git-svn-map-svn-path (car birth))
						  parent-rev))
		(setq line (plist-get res :line))
		(unless (plist-get res :success) 
		  (message "Failed to unfetch and refetch svn revisions back from r%d: %s"
			   parent-rev line)
		  (throw 'full-ref nil)))
	    (message "Cancelled before unfetch and refetch old revisions!")
	    (throw 'full-ref nil)))

	;; at this point, a fetch might have already been done
	;; (re-read max-rev). Thus, the branch might already imported
	;; by git-svn.
	(setq max-rev (egg-git-svn-max-rev svn-name))
	
	(if (egg-svn-is-known-ref full-ref)
	    full-ref
	  ;; ok, the ref is still unknown by git-svn. try a normal fetch
	  (message "fetching %s -> %s..." svn-path full-ref)
	  (setq res (egg-git-svn-do-fetch buffer-to-update svn-name svn-path full-ref))
	  (setq line (plist-get res :line))
	  (unless (plist-get res :success)
	    (message "Failed to fetch svn revisions up to r%d: %s" branch-last-rev line)
	    (throw 'full-ref nil))
	  ;; didn't catch the new rev with a regular fetch, try fetch the exact rev
	  ;; get the max-rev after the previous regular fetch
	  (message "fetching r%d..." branch-first-rev)
	  (setq res (egg--git-svn-fetch-rev buffer-to-update svn-name branch-first-rev))
	  (setq line (plist-get res :line))
	  (unless (plist-get res :success)
	    (message "Failed to fetch svn r%d: %s" branch-first-rev line)
	    (throw 'full-ref nil))

	  (setq max-rev (egg-git-svn-max-rev svn-name))
	  (unless (egg-svn-is-known-ref full-ref)
	    (message "Bailed out: git-svn refused to fetch r%d (git-svn maxRev=%d)"
		     branch-first-rev max-rev)
	    (throw 'full-ref nil))
	  ;; verify
	  (setq new-rev (egg-git-to-string "svn" "find-rev" full-ref))
	  (setq sha1 (egg-git-to-string "rev-parse" full-ref))
	  (if (and new-rev sha1
		   (equal (egg-git-to-string "svn" "find-rev" 
					     (concat "r" new-rev) full-ref) sha1))
	      full-ref
	    (message "Failed fetching %s to %s" svn-path full-ref)
	    (throw 'full-ref nil)))))))


(defun egg-fetch-from-svn (buffer-to-update svn-remote r-ref)
  (let* ((r-ref (if (equal r-ref "--all") nil r-ref))
	 (svn-name (car svn-remote))
	 (map-type (nth 1 svn-remote))
	 (url (and svn-name (egg-git-svn-url svn-name)))
	 (svn-prefix (nth 2 svn-remote))
	 (local-base (nth 3 svn-remote))
	 full-ref short-ref svn-path svn-path-url local-name
	 max-rev path-last-rev ref-fetched-rev res line fetch-unknown)

    (save-match-data
      (cond ((null r-ref)
	     ;; --all
	     (setq svn-path-url url)
	     (setq local-name "everything"))

	     ((string-match "\\`refs/remotes/" r-ref)
	      ;; r-ref is fully-named ref
	      (setq full-ref r-ref)
	      (setq svn-path (egg-git-svn-map-full-ref full-ref)))

	     ((string-match "/" r-ref)
	      ;; r-ref was branches/user7/something
	      (setq svn-path r-ref)
	      (setq full-ref (egg-git-svn-map-svn-path svn-path))
	      nil)

	     ((eq map-type :fetch)
	      ;; cursor was on svn/xxxx and r-ref is foo	      
	      (if (egg-svn-is-known-ref (concat (file-name-directory local-base) r-ref))
		  (setq full-ref (concat (file-name-directory local-base) r-ref)
			svn-path (setq svn-path (egg-git-svn-map-full-ref full-ref)))
		(setq svn-path r-ref)
		(setq full-ref (egg-git-svn-map-svn-path svn-path))))

	     ((eq map-type :branches)
	      ;; cursor was on user1/xxxx and r-ref is bar
	      (if (egg-svn-is-known-ref (concat local-base r-ref))
		  (setq full-ref (concat local-base r-ref)
			svn-path (egg-git-svn-map-full-ref full-ref))
		(setq svn-path (concat svn-prefix r-ref))
		(setq full-ref (egg-git-svn-map-svn-path svn-path))))

	     (t (error "Cannot determine svn path from %s" r-ref))))


    (when (and (not svn-path-url) svn-path)
      (setq svn-path-url (concat url "/" svn-path)))

    (if (and r-ref (or (not full-ref) (not (egg-svn-is-known-ref full-ref))))
	(setq full-ref (egg-fetch-unknown-svn-path buffer-to-update svn-name svn-path
						   local-base))
      (setq path-last-rev (egg-svn-path-last-rev svn-path-url))
      (setq max-rev (egg-git-svn-max-rev svn-name))

      (unless local-name (setq local-name full-ref))

      (if (<= path-last-rev 
	      (if full-ref (egg-git-svn-ref-last-rev full-ref) max-rev))
	  (message "%s is already up-to-date, no fetching required!" local-name)
	;; regular fetch
	(message "need %s@%d, fetching..." svn-path-url path-last-rev)
	(setq res (egg--git-svn-fetch buffer-to-update svn-name))
	(setq line (plist-get res :line))
	(unless (plist-get res :success)
	  (error "Failed to fetch svn revisions from r%d to r%d: %s" max-rev 
		 path-last-rev line))
	(setq max-rev (egg-git-svn-max-rev svn-name))
	(if (<= path-last-rev 
		(if full-ref
		    (setq ref-fetched-rev (egg-git-svn-ref-last-rev full-ref))
		  max-rev))
	    (message "%s is now up-to-date, no more fetching required!" local-name)
	  (if (not (and full-ref svn-path))
	      ;; general fetch, give up after one try
	      (message "git-svn refused to fetch %s@%d!" svn-path-url path-last-rev)
	    ;; try with temporary mapping
	    (message "retry fetching %s -> %s..." svn-path full-ref)
	    (setq res (egg-git-svn-do-fetch buffer-to-update svn-name svn-path full-ref))
	    (setq line (plist-get res :line))
	    (unless (plist-get res :success)
	      (error "Failed to fetch %s (%s): %s" svn-path full-ref line))
	    (setq ref-fetched-rev (egg-git-svn-ref-last-rev full-ref))
	    (if (>= ref-fetched-rev path-last-rev)
		(message "%s is now update-to-date (used temporary mapping during fetching)!" 
			 local-name)
	      ;; try with explicit rev -> BAD!!!!
	      (message "retry fetching r%d..." path-last-rev)
	      (setq res (egg--git-svn-fetch-rev buffer-to-update svn-name path-last-rev))
	      (setq line (plist-get res :line))
	      (unless (plist-get res :success)
		(error "Failed to fetch r%d: %s" path-last-rev line))
	      (setq ref-fetched-rev (string-to-number (egg-git-svn-ref-last-rev full-ref)))
	      (if (>= ref-fetched-rev path-last-rev)
		  (message "managed to (explicitly) fetch r%d, %s is now update-to-date!" 
			   path-last-rev local-name)
		(message "Giving up! git-svn refused to fetch %s up to r%d" 
			 svn-path path-last-rev)))))))))



(add-to-list 'egg-add-remote-properties #'egg-svn-add-remote-properies)
(add-to-list 'egg-get-remote-properties #'egg-svn-get-remote-properies)
(add-to-list 'egg-get-all-remotes #'egg-svn-get-all-prefixes)

(provide 'egg-svn)
