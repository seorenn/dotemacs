;;; egg-diff.el --- Emacs Got Git - Emacs interface to Git

;; Copyright (C) 2008  Linh Dang
;;
;; Author: Bogolisk <bogolisk@gmail.com>
;; Created: 02 Nov 2012
;; Keywords: git, version control, release management
;;
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

(require 'egg)

(defcustom egg-inline-diff-hide-non-context-text nil
  "When entering inline-diff mode, hide all non-context lines by default."
  :group 'egg
  :type 'boolean)

(defconst egg-file-inline-diff-map
  (let ((map (make-sparse-keymap "Egg:FileInlineDiff")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "h") 'egg-inline-diff-toggle-hide-show)
    (define-key map (kbd "g") 'egg-inline-diff-refresh-file-buffer)
    (define-key map (kbd "q") 'egg-file-quit-inline-diff-mode)
    (define-key map (kbd "C-x v d") 'egg-buffer-toggle-inline-diff)
    map)
  "Keymap for inline diff mode.
\\{egg-file-inline-diff-map}")

(defconst egg-file-inline-diff-block-map
  (let ((map (make-sparse-keymap "Egg:FileInlineDiffBlock")))
    (set-keymap-parent map egg-file-inline-diff-map)
    (define-key map (kbd "s") 'egg-inline-diff-stage)
    (define-key map (kbd "u") 'egg-inline-diff-undo)
    map)
  "Keymap for a block of delta of a WorkTree's file in inline diff mode.
\\{egg-inline-diff-block-map}")

(defconst egg-index-inline-diff-map
  (let ((map (make-sparse-keymap "Egg:InexInlineDiff")))
    (set-keymap-parent map egg-basic-map)
    (define-key map (kbd "h") 'egg-inline-diff-toggle-hide-show)
    (define-key map (kbd "g") 'egg-inline-diff-refresh-index-buffer)
    (define-key map (kbd "q") 'egg-index-quit-inline-diff-mode)
    (define-key map (kbd "d") 'egg-index-toggle-inline-diff)
    (define-key map (kbd "C-x v d") 'egg-index-toggle-inline-diff)
    map)
  "Keymap for inline diff mode of an index buffer.
\\{egg-index-inline-diff-map}")

(defconst egg-index-inline-diff-block-map
  (let ((map (make-sparse-keymap "Egg:IndexInlineDiffBlock")))
    (set-keymap-parent map egg-index-inline-diff-map)
    (define-key map (kbd "s") 'egg-inline-diff-unstage)
    (define-key map (kbd "S") 'egg-inline-diff-index-reset)
    map)
  "Keymap for a block of delta of a WorkTree's file in inline diff mode.
\\{egg-inline-diff-block-map}")

(defvar egg-inline-diff-info nil)

(defun egg-inline-diff-toggle-hide-show ()
  (interactive)
  (if (member '(0 . t) buffer-invisibility-spec)
      (setq buffer-invisibility-spec (remove '(0 . t) buffer-invisibility-spec))
    (push '(0 . t) buffer-invisibility-spec)))

(defun egg--inline-diff-mk-patch (file line del-num add-num del-text add-text)
  (let ((pre-lines 0)
	(post-lines 0)
	(del-num (or del-num 0))
	(add-num (or add-num 0))
	pre post beg end text)

    (goto-char (egg-line-2-pos line))
    (while (and (not (bobp)) (< pre-lines 3))
      (forward-line -1)
      (setq pre-lines (1+ pre-lines)))
    (setq beg (point))
    (goto-char (egg-line-2-pos line))
    (if del-text
	(insert del-text)
      (forward-line del-num))
    (if add-text
	(insert add-text)
      (forward-line add-num))
    (while (and (not (eobp)) (< post-lines 3))
      (forward-line 1)
      (setq post-lines (1+ post-lines)))
    (setq end (point))
    (setq text (buffer-substring-no-properties beg end))

    (with-temp-buffer
      (insert (format "diff a/%s b/%s\n" file file)
	      (format "--- a/%s\n+++ b/%s\n" file file)
	      (format "@@ -%d,%d +%d,%d @@\n"
		      (- line pre-lines)
		      (+ pre-lines del-num post-lines)
		      (- line pre-lines)
		      (+ pre-lines add-num post-lines)))
      (setq beg (point))
      (insert text)
      (setq end (point))
      (goto-char beg)
      (dotimes (i pre-lines)
	(insert " ")
	(forward-line 1))
      (dotimes (i del-num)
	(insert "-")
	(forward-line 1))
      (dotimes (i add-num)
	(insert "+")
	(forward-line 1))
      (dotimes (i post-lines)
	(insert " ")
	(forward-line 1))
      (buffer-string))))

(defun egg--inline-diff-block2patch (file-git-name rev del-pos add-pos)
  (let* (
	 ;; (file (buffer-file-name))
	 ;; (file-git-name (egg-file-file-git-name file))
	 
	 (dir (egg-work-tree-dir))
	 (new-del-line (and del-pos (- (get-text-property del-pos :navigation))))
	 (new-add-line (and add-pos (get-text-property add-pos :navigation)))
	 (old-del-line (and del-pos (get-text-property del-pos :old-line)))
	 (old-add-line (and add-pos (get-text-property add-pos :orig-line)))
	 (del-num (and del-pos (get-text-property del-pos :num)))
	 (add-num (and add-pos (get-text-property add-pos :num)))
	 text text-info patch
	 has-new-text buffer-contents)

    (cond ((and (consp rev) (equal (car rev) ":0"))
	   ;; index vs rev
	   ;; apply patch to index
	   ;; index is the newer copy, rev is the older copy
	   (setq has-new-text t)
	   (setq buffer-contents :index))
	  ((equal rev ":0")
	   ;; worktree vs index
	   ;; applying to index
	   ;; index is the older copy, worktree is the newer copy
	   (setq has-new-text nil)
	   (setq buffer-contents :index))
	  ((null rev)
	   ;; worktree vs a revision
	   ;; applying to worktree
	   ;; the revision the older copy, worktree is the newer copy
	   (setq has-new-text t)
	   (setq buffer-contents :file))
	  (t
	   (error "Cannot handle rev: %s" rev)))
    
    (if has-new-text
	(progn
	  ;; no revision, mean applying to worktree's file
	  (unless (or new-del-line new-add-line)
	    (error "No info found to build patch: positions %d/%d" del-pos add-pos))
	  
	  (unless (or (null new-del-line) (null new-add-line)
		      (= new-del-line new-add-line))
	    (error "Cannot merge separate diff blocks: -%d and +%d" 
		   new-del-line new-add-line))
	  (when del-pos
	    (setq text-info 
		  (assq (- new-del-line) (plist-get egg-inline-diff-info :text-positions)))))
      (progn
	(unless (or old-del-line old-add-line)
	  (error "No info found to build patch: positions %d/%d" del-pos add-pos))
	  
	(unless (or (null old-del-line) (null old-add-line)
		    (= (+ old-del-line del-num) old-add-line))
	  (error "Cannot merge separate diff blocks: -%d and +%d" 
		 old-del-line old-add-line))
	(when add-pos
	  (setq text-info 
		(assq new-add-line (plist-get egg-inline-diff-info :text-positions))))))

    (when text-info
      (setq text (buffer-substring-no-properties (nth 1 text-info)
						 (nth 2 text-info))))

    ;; (with-current-buffer "*bar*"
    ;;   (erase-buffer)
    (with-temp-buffer
      (setq default-directory dir)
      (cond ((eq buffer-contents :index)
	     (egg-git-ok t "--no-pager" "show" (concat ":0:" file-git-name)))
	    ((eq buffer-contents :file)
	     (insert-file-contents file-git-name)))
      (setq patch
	    (if has-new-text
		;; already has new text in buffer, pass the old text in
		(egg--inline-diff-mk-patch file-git-name (or new-del-line new-add-line)
					   del-num add-num text nil)
	      ;; has old text in buffer, pass the new text in
	      (egg--inline-diff-mk-patch file-git-name (or old-del-line old-add-line)
					 del-num add-num nil text))))

    (egg-cmd-log "PATCH BEGIN: " file-git-name 
		 (format " rev:%s %s/%s\n" rev del-pos add-pos)
		 patch)
    (egg-cmd-log "PATCH END\n")

    patch))

(defun egg-inline-diff-compute-info ()
  (let (hunk beg ranges all text-beg block-lines prev-line-no
	     block-start line-no old-start old-line-no
	     current-prefix start-c) 
    (goto-char (point-max))
    (save-match-data
      (while (re-search-backward "^@@@? .+ @@@?" nil t)
	(setq beg (match-beginning 0))
	(setq hunk (mapcar #'string-to-number
			   (split-string (match-string-no-properties 0) "[-+,@ ]+" t)))
	(setq line-no (1- (nth 2 hunk)))
	(setq old-line-no (1- (nth 0 hunk)))
	(setq old-start nil)
	(while (not (eobp))
	  (forward-line 1)
	  (setq start-c (char-after (point)))
	  (unless (eobp) (delete-char 1))

	  ;; keep the prev-line-no because
	  ;; line-no is conditionally incremented
	  (setq prev-line-no line-no)
	  (when (memq start-c '(?- ? )) (setq old-line-no (1+ old-line-no)))
	  (when (memq start-c '(?+ ? )) (setq line-no (1+ line-no)))

	  (if (eq current-prefix start-c)
	      (setq block-lines (1+ block-lines))

	    ;; switching
		
	    ;; building info
	    (cond ((eq current-prefix ? ) nil)
		  ((eq current-prefix ?+)
		   (push (list :add block-start block-lines old-start) ranges))
		  ((eq current-prefix ?-)
		   (push (list :del block-start block-lines old-start
			       (buffer-substring-no-properties text-beg (point)))
			 ranges)))

	    (cond ((eq start-c ? )
		   ;; restart position in the original file
		   (setq old-start nil))
		  ((eq start-c ?-)
		   ;; keep removed text because it's not in the file
		   (setq text-beg (point))
		   ;; del-only or combined hunk, this is the starting point
		   ;; of the changes in the original file
		   (setq old-start old-line-no))
		  ((eq start-c ?+)
		   ;; the strting point in the original file is either:
		   ;;  - the beginning of the add-only lines
		   ;;  - or the end of the del-lines
		   ;; +1 because old-line-no doesn't advance this time
		   (setq old-start (1+ old-line-no))))

	    ;; start new block
	    (setq block-lines 1)
	    (setq current-prefix start-c)
	    ;; in the case of del block, line-no would have stayed the same
	    ;; but we must move the block start ahead
	    (setq block-start (1+ prev-line-no))))
	;; do full block
	(push (list :same (nth 2 hunk) (nth 3 hunk)) ranges)
	;; done current ranges
	(setq all (nconc all ranges))
	(setq ranges nil)
	(goto-char beg)
	(delete-region beg (point-max))))
    all))

(defun egg-inline-diff-compute-file-info (file-name)
  (let (no-diff) 
    (with-temp-buffer
      (setq no-diff
	    (egg-git-ok t "--no-pager" "diff" "--exit-code" 
			;;"--function-context"
			;;"--unified=5"
			"--" file-name))
      (unless no-diff
	(egg-inline-diff-compute-info)))))

(defun egg-inline-diff-compute-index-info (file-name)
  (let (no-diff) 
    (with-temp-buffer
      (setq no-diff
	    (egg-git-ok t "--no-pager" "diff" "--exit-code" "--cached" "HEAD"
			;;"--function-context"
			;;"--unified=5"
			"--" file-name))
      (unless no-diff
	(egg-inline-diff-compute-info)))))

(defun egg-line-2-pos (line &optional num)
  (save-excursion
    (setq num (or num 0))
    (goto-char (point-min))
    (forward-line (1- (+ line num)))
    (point)))

(defun egg--inline-diff-mk-boundaries-visible (beg end)
  (when (and (> beg (point-min)) (eq (char-before beg) ?\n))
    (put-text-property (1- beg) beg 'invisible nil))
  (when (eq (char-before end) ?\n)
    (put-text-property (1- end) end 'invisible nil)))


(defun egg-do-buffer-inline-diff (ranges common-diff-map diff-block-map)
  (let ((read-only-state buffer-read-only)
	(inhibit-read-only t)
	beg end
	type line num text old-line
	text-list text-list ov-list ov)
    (widen)
    (setq buffer-invisibility-spec nil)
    (add-text-properties (point-min) (point-max)
			 (list :navigation 0
			       'invisible 0
			       'keymap common-diff-map))
    (dolist (range ranges)
      (setq type (nth 0 range))
      (setq line (nth 1 range))
      (setq num (nth 2 range))
      (setq old-line (nth 3 range))
      (setq text (nth 4 range))
      (setq beg (egg-line-2-pos line))
      (setq end (egg-line-2-pos line num))
      (cond ((eq type :same) 
	     (add-text-properties beg end
				  (list :navigation 0
					'invisible nil
					'keymap common-diff-map))
	     (egg--inline-diff-mk-boundaries-visible beg end))
	    ((eq type :add)
	     (add-text-properties beg end
				  (list :navigation line
					'invisible nil
					:orig-line old-line
					:num num
					'keymap diff-block-map))
	     (egg--inline-diff-mk-boundaries-visible beg end)
	     (setq ov (make-overlay beg end nil t nil))
	     (overlay-put ov 'face 'egg-add-bg)
	     (overlay-put ov 'evaporate t)
	     (push ov ov-list)
	     (setq beg (copy-marker beg t))
	     (setq end (copy-marker end t))
	     (push (list line beg end) text-list))
	    ((eq type :del)
	     (goto-char beg)
	     (insert text)
	     (setq end (point))
	     (add-text-properties beg end 
				  (list :navigation (- line)
					'invisible nil
					:old-line old-line
					:num num
					'keymap diff-block-map))
	     (egg--inline-diff-mk-boundaries-visible beg end)
	     (setq ov (make-overlay beg end nil t nil))
	     (overlay-put ov 'face 'egg-del-bg)
	     (overlay-put ov 'evaporate t)
	     (push ov ov-list)
	     (setq beg (copy-marker beg t))
	     (setq end (copy-marker end t))
	     (push (list (- line) beg end) text-list))))
    (when egg-inline-diff-hide-non-context-text
      (setq buffer-invisibility-spec (list '(0 . t))))
    (set (make-local-variable 'egg-inline-diff-info)
	 (list :read-only read-only-state
	       :overlays ov-list 
	       :text-positions text-list))
    (egg-set-global-mode " Egg:inDiff")
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

(defun egg-undo-buffer-inline-diff ()
  (let ((overlays (plist-get egg-inline-diff-info :overlays))
	(text-positions (plist-get egg-inline-diff-info :text-positions))
	(inhibit-read-only t))
    (widen)
    (dolist (ov overlays) (delete-overlay ov))
    (remove-text-properties (point-min) (point-max)
			    '(:navigation nil invisible nil :num nil
					  :old-line nil :orig-line nil
					  'keymap nil))
    ;; emacs's bug, keymap must be removed separately
    (remove-text-properties (point-min) (point-max) '(keymap nil))
    (dolist (pos text-positions)
      (when (< (nth 0 pos) 0)
	(delete-region (nth 1 pos) (nth 2 pos)))
      (set-marker (nth 1 pos) nil)
      (set-marker (nth 2 pos) nil))
    (setq buffer-read-only (plist-get egg-inline-diff-info :read-only))
    (egg-set-global-mode)
    (set-buffer-modified-p nil)
    (setq egg-inline-diff-info nil)))

(defun egg-file-quit-inline-diff-mode ()
  (interactive)
  (egg-undo-buffer-inline-diff))

(defun egg-index-quit-inline-diff-mode ()
  (interactive)
  (let ((buffer (current-buffer)))
    (bury-buffer buffer)
    (kill-buffer buffer)))

(defun egg-buffer-toggle-inline-diff ()
  (interactive)
  (if egg-inline-diff-info
      (egg-undo-buffer-inline-diff)
    (let ((ranges (egg-inline-diff-compute-file-info (buffer-file-name))))
      (if ranges
	  (egg-do-buffer-inline-diff ranges 
				     egg-file-inline-diff-map 
				     egg-file-inline-diff-block-map)
	(message "no differences in %s" (buffer-file-name))))))

(defun egg-index-toggle-inline-diff ()
  (interactive)
  (widen)
  (if egg-inline-diff-info
      (let ((inhibit-read-only t))
	(egg-undo-buffer-inline-diff)
	(put-text-property (point-min) (point-max) 'keymap
			   egg-file-index-map)
	(egg-set-global-mode " Egg")
	(set-buffer-modified-p nil))
    (let ((ranges (egg-inline-diff-compute-index-info egg-git-name)))
      (if ranges
	  (egg-do-buffer-inline-diff ranges 
				     egg-index-inline-diff-map
				     egg-index-inline-diff-block-map)
	(message "no differences in %s" (buffer-file-name))))))

(defun egg--inline-diff-figure-out-pos (pos del-prompt add-prompt)
  (let (add-pos del-pos)
    (cond ((< (get-text-property pos :navigation) 0)
	   (setq del-pos pos)
	   (setq pos (next-single-property-change pos :navigation))
	   (when (and (get-text-property pos :orig-line)
		      (y-or-n-p add-prompt))
	     (setq add-pos pos)))
	  ((> (get-text-property pos :navigation) 0)
	   (setq add-pos pos)
	   (setq pos (or (and (not (eobp))
			      (get-text-property (1- pos) :old-line)
			      pos)
		      (previous-single-property-change pos :navigation)))
	   (when (and pos 
		      (not (bobp)) 
		      (get-text-property (1- pos) :old-line)
		      (y-or-n-p del-prompt))
	     (setq del-pos (1- pos)))))
    (if (or del-pos add-pos)
	(cons del-pos add-pos)
      nil)))


(defun egg--inline-diff-file-update-index-buffer ()
  (let ((buffer (egg-file-get-index-buffer))
	win)
    (when (and (bufferp buffer) (buffer-live-p buffer))
      (with-current-buffer buffer
	(when egg-inline-diff-info
	  (setq win (get-buffer-window buffer))
	  (if win
	      (with-selected-window win
		(egg-inline-diff-refresh-index-buffer))
	    (egg-inline-diff-refresh-index-buffer)))))))

(defun egg--inline-diff-index-update-file-buffer ()
  (let ((buffer (egg-index-get-file-visiting-buffer))
	win)
    (when (and (bufferp buffer) (buffer-live-p buffer))
      (with-current-buffer buffer
	(when egg-inline-diff-info
	  (setq win (get-buffer-window buffer))
	  (if win
	      (with-selected-window win
		(egg-inline-diff-refresh-file-buffer))
	    (egg-inline-diff-refresh-file-buffer)))))))


(defun egg-inline-diff-refresh-file-buffer ()
  (interactive)
  (let ((line (line-number-at-pos))
	(invisiblity-spec buffer-invisibility-spec))
    (egg-undo-buffer-inline-diff)
    (revert-buffer t t t)
    (egg-do-buffer-inline-diff (egg-inline-diff-compute-file-info (buffer-file-name))
			       egg-file-inline-diff-map egg-file-inline-diff-block-map)
    (goto-char (point-min))
    (forward-line (1- line))
    (setq buffer-invisibility-spec invisiblity-spec)))

(defun egg-inline-diff-refresh-index-buffer ()
  (interactive)
  (let ((line (line-number-at-pos))
	(invisiblity-spec buffer-invisibility-spec))
    (egg-undo-buffer-inline-diff)
    (egg-file-get-other-version egg-git-name egg-git-revision nil t)
    (egg-do-buffer-inline-diff (egg-inline-diff-compute-index-info egg-git-name)
			        egg-index-inline-diff-map egg-index-inline-diff-block-map)
    (goto-char (point-min))
    (forward-line (1- line))
    (setq buffer-invisibility-spec invisiblity-spec)))

(defun egg-inline-diff-stage (pos)
  (interactive "d")
  (unless (get-text-property pos :num)
    (error "Nothing here to stage!"))
  (let ((invisibility-spec buffer-invisibility-spec)
	anchor patch res line)
    (setq anchor (egg--inline-diff-figure-out-pos 
		  pos "stage the preceding del block as well? "
		  "stage the following add block as well? "))
    (unless anchor
      (error "Failed to grok position %d" pos))
    (setq patch (egg--inline-diff-block2patch (egg-file-git-name (buffer-file-name))
					      ":0" (car anchor) (cdr anchor)))
    (setq res (egg--git-apply-cmd t patch (list "--cached")))
    (when (plist-get res :success)
      (setq line (get-text-property pos :navigation))
      (when (< line 0)
	(setq line (- line)))
      (egg-undo-buffer-inline-diff)
      (revert-buffer t t t)
      (setq pos (copy-marker (egg-line-2-pos line)))
      (egg-do-buffer-inline-diff (egg-inline-diff-compute-file-info (buffer-file-name))
				 egg-file-inline-diff-map egg-file-inline-diff-block-map)
      (setq buffer-invisibility-spec invisibility-spec)
      (goto-char pos)
      (set-marker pos nil)
      (egg--inline-diff-file-update-index-buffer))))

(defun egg-inline-diff-unstage (pos)
  (interactive "d")
  (unless (get-text-property pos :num)
    (error "Nothing here to stage!"))
  (let ((invisibility-spec buffer-invisibility-spec)
	(inhibit-read-only t)
	anchor patch res line)
    (setq anchor (egg--inline-diff-figure-out-pos 
		  pos "unstage the preceding del block as well? "
		  "unstage the following add block as well? "))
    (unless anchor
      (error "Failed to grok position %d" pos))
    (setq patch (egg--inline-diff-block2patch egg-git-name '(":0". "HEAD")
					      (car anchor) (cdr anchor)))
    (setq res (egg--git-apply-cmd t patch (list "--cached" "--reverse")))
    (when (plist-get res :success)
      (setq line (get-text-property pos :navigation))
      (when (< line 0)
	(setq line (- line)))
      (egg-undo-buffer-inline-diff)
      (egg-file-get-other-version egg-git-name egg-git-revision nil t)
      (setq pos (copy-marker (egg-line-2-pos line)))
      (egg-do-buffer-inline-diff (egg-inline-diff-compute-index-info egg-git-name)
			        egg-index-inline-diff-map egg-index-inline-diff-block-map)
      (setq buffer-invisibility-spec invisibility-spec)
      (goto-char pos)
      (set-marker pos nil)
      (egg--inline-diff-index-update-file-buffer))))

(defun egg-inline-diff-undo (pos)
  (interactive "d")
  (unless (get-text-property pos :num)
    (error "Nothing here to undo!"))
  (let ((invisibility-spec buffer-invisibility-spec)
	anchor patch res line)
    (setq anchor (egg--inline-diff-figure-out-pos 
		  pos "undo the preceding del block as well? "
		  "undo the following add block as well? "))
    (unless anchor
      (error "Failed to grok position %d" pos))
    (setq patch (egg--inline-diff-block2patch (egg-file-git-name (buffer-file-name))
					      nil (car anchor) (cdr anchor)))
    (setq line (get-text-property pos :navigation))
    (if (< line 0) (setq line (- line)))
    (egg-undo-buffer-inline-diff)
    (setq res (egg--git-apply-cmd t patch (list "--reverse")))
    (revert-buffer t t t)
    (when (plist-get res :success)
      (setq pos (copy-marker (egg-line-2-pos line))))
    (egg-do-buffer-inline-diff (egg-inline-diff-compute-file-info (buffer-file-name))
			       egg-file-inline-diff-map egg-file-inline-diff-block-map)
    (setq buffer-invisibility-spec invisibility-spec)
    (goto-char pos)
    (when (markerp pos)
      (set-marker pos nil))))


(define-key egg-file-cmd-map (kbd "d") 'egg-buffer-toggle-inline-diff)
(define-key egg-file-index-map (kbd "d") 'egg-index-toggle-inline-diff)

(provide 'egg-diff)
