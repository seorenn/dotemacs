;;; egg-custom.el --- Emacs Got Git - Emacs interface to Git
;;
;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2011-12 byplayer
;; Author: Bogolisk <bogolisk@gmail.com>
;; Created: 27 Oct 2012
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

(defgroup egg nil
  "Controlling Git from Emacs."
  :prefix "egg-"
  :group 'tools)

(defgroup egg-faces nil
  "Colourful Faces for Egg."
  :group 'egg)

(defface egg-header
  '((t :weight bold :inherit variable-pitch :height 1.1))
  "Face for generic headers.

Many Egg faces inherit from this one by default."
  :group 'egg-faces)

(defface egg-text-base
  '((((class color) (background light))
     :foreground "navy" :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "SteelBlue" :inherit variable-pitch)
    (t))
  "Face for description text."
  :group 'egg-faces)

(defface egg-text-1
  '((t :inherit egg-text-base))
  "Face for description text."
  :group 'egg-faces)

(defface egg-text-help
  '((t :inherit egg-text-base :height 0.8))
  "Face for help text."
  :group 'egg-faces)

(defface egg-help-header-1
  '((t :inherit egg-text-base :weight bold))
  "Face for help text."
  :group 'egg-faces)

(defface egg-help-header-2
  '((((class color) (background light))
     :foreground "Black" :inherit egg-text-1 :height 0.9)
    (((class color) (background dark))
     :foreground "LightSteelBlue" :inherit egg-text-1 :height 0.9)
    (t :inherit egg-text-1))
  "Face for help text."
  :group 'egg-faces)

(defface egg-text-2
  '((t :inherit egg-text-base :height 1.1))
  "Face for description text."
  :group 'egg-faces)

(defface egg-text-3
  '((t :inherit egg-text-base :height 1.2))
  "Face for description text."
  :group 'egg-faces)

(defface egg-text-4
  '((t :inherit egg-text-base :height 1.4))
  "Face for description text."
  :group 'egg-faces)

(defface egg-electrict-choice
  '((((class color) (background light))
     :foreground "Blue" :inherit egg-text-1 :weight bold)
    (((class color) (background dark))
     :foreground "Cyan" :inherit egg-text-1 :weight bold)
    (t))
  "Face for description text."
  :group 'egg-faces)

(defface egg-section-title
  '((((class color) (background light))
     :foreground "DarkGoldenrod" :inherit egg-header :height 1.1)
    (((class color) (background dark))
     :foreground "PaleGreen" :inherit egg-header :height 1.1)
    (t :weight bold))
  "Face for generic header lines.

Many Egg faces inherit from this one by default."
  :group 'egg-faces)

(defface egg-branch
  '((((class color) (background light))
     :foreground "SkyBlue" :inherit egg-header :height 1.1)
    (((class color) (background dark))
     :foreground "Yellow" :inherit egg-header :height 1.1)
    (t :weight bold))
  "Face for the current branch."
  :group 'egg-faces)

(defface egg-log-buffer-mark
  '((((class color) (background light))
     :foreground "black" :inherit bold)
    (((class color) (background dark))
     :foreground "orchid1" :inherit bold)
    (t :weight bold))
  "Face to mark commit line in log-buffer."
  :group 'egg-faces)

(defface egg-branch-mono
  '((((class color) (background light))
     :foreground "SkyBlue" :inherit bold)
    (((class color) (background dark))
     :foreground "Yellow" :inherit bold)
    (t :weight bold))
  "Face for a branch."
  :group 'egg-faces)

(defface egg-tag-mono
  '((((class color) (background light))
     :foreground "GoldenRod" :inherit bold)
    (((class color) (background dark))
     :foreground "SkyBlue" :inherit bold)
    (t :weight bold))
  "Face for a tag."
  :group 'egg-faces)

(defface egg-an-tag-mono
  '((((class color) (background light))
     :foreground "DarkGoldenRod" :inherit bold)
    (((class color) (background dark))
     :foreground "LightGreen" :inherit bold)
    (t :weight bold))
  "Face for an annotated branch."
  :group 'egg-faces)

(defface egg-stash-mono
  '((((class color) (background light))
     :foreground "DarkGoldenRod" :inherit bold)
    (((class color) (background dark))
     :foreground "LightGreen" :inherit bold)
    (t :weight bold))
  "Face for a stash identifier."
  :group 'egg-faces)

(defface egg-reflog-mono
  '((((class color) (background light))
     :foreground "gray70" :inherit egg-stash-mono)
    (((class color) (background dark))
     :foreground "gray30" :inherit egg-stash-mono)
    (t :weight bold))
  "Face for a reflog identifier."
  :group 'egg-faces)

(defface egg-low-prio-mono-text
  '((((class color) (background light))
     :foreground "gray70" :inherit egg-stash-mono)
    (((class color) (background dark))
     :foreground "gray30" :inherit egg-stash-mono)
    (t :weight bold))
  "Face low priority monospaced text."
  :group 'egg-faces)

(defface egg-remote-mono
  '((((class color) (background light))
     :foreground "Orchid" :inherit bold)
    (((class color) (background dark))
     :foreground "DarkSalmon" :inherit bold)
    (t :weight bold))
  "Face for a remote."
  :group 'egg-faces)

(defface egg-term
  '((((class color) (background light))
     :foreground "SkyBlue" :inherit bold)
    (((class color) (background dark))
     :foreground "Yellow" :inherit bold)
    (t :weight bold))
  "Face for an important term."
  :group 'egg-faces)

(defface egg-help-key
  '((t :inherit egg-term :height 0.9))
  "Hilight Face in help text."
  :group 'egg-faces)

(defface egg-warning
  '((((class color) (background light))
     :foreground "Red" :inherit bold)
    (((class color) (background dark))
     :foreground "Orange" :inherit bold)
    (t :weight bold))
  "Face for a warning."
  :group 'egg-faces)

(defface egg-diff-file-header
  '((((class color) (background light))
     :foreground "SlateBlue" :inherit egg-header)
    (((class color) (background dark))
     :foreground "LightSlateBlue" :inherit egg-header)
    (t :weight bold))
  "Face for diff file headers."
  :group 'egg-faces)

(defface egg-unmerged-diff-file-header
  '((((class color) (background light))
     :foreground "Red" :inherit egg-diff-file-header)
    (((class color) (background dark))
     :foreground "Orange" :inherit egg-diff-file-header)
    (t :weight bold))
  "Face for unmerged diff file headers."
  :group 'egg-faces)

(defface egg-diff-hunk-header
  '((((class color) (background light))
     :background "grey85")
    (((class color) (background dark))
     :background "grey45"))
  "Face for diff hunk headers."
  :group 'egg-faces)

(defface egg-conflict-resolution
  '((((class color) (background light))
     :background "grey95")
    (((class color) (background dark))
     :background "grey35"))
  "Face highlighting the resolution selection."
  :group 'egg-faces)

(defface egg-diff-add
  '((((class color) (background light))
     :foreground "blue1")
    (((class color) (background dark))
     :foreground "ForestGreen"))
  "Face for lines in a diff that have been added."
  :group 'egg-faces)

(defface egg-add-bg
  '((((class color) (background light))
     :background "medium sea green")
    (((class color) (background dark))
     :background "#375243"))
  "Background Face for lines in a diff that have been added."
  :group 'egg-faces)

(defface egg-del-bg
  '((((class color) (background light))
     :background "indian red")
    (((class color) (background dark))
     :background "IndianRed4"))
  "Background Face for lines in a diff that have been deleted."
  :group 'egg-faces)



(defface egg-diff-none
  '((((class color) (background light))
     :foreground "grey50")
    (((class color) (background dark))
     :foreground "grey70"))
  "Face for lines in a diff that are unchanged."
  :group 'egg-faces)

(defface egg-diff-del
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "OrangeRed"))
  "Face for lines in a diff that have been deleted."
  :group 'egg-faces)

(defface egg-diff-conflict
  '((((class color) (background light))
     :foreground "Blue")
    (((class color) (background dark))
     :foreground "Orange"))
  "Face for lines in a diff that have been deleted."
  :group 'egg-faces)

(defface egg-graph
  '((((class color) (background light))
     :foreground "grey90")
    (((class color) (background dark))
     :foreground "grey30"))
  "Face for graph."
  :group 'egg-faces)

(defface egg-blame
  '((((class color) (background light))
     :background: "grey85" :foreground "black")
    (((class color) (background dark))
     :background "grey15" :foreground "white")
    (t :inherit region))
  "Face for blame header."
  :group 'egg-faces)

(defface egg-blame-culprit
  '((((class color) (background light))
     :inherit egg-text-2 :background "grey85" :foreground "grey35")
    (((class color) (background dark))
     :inherit egg-text-2 :background "grey15" :foreground "grey60")
    (t :inherit egg-blame))
  "Face for blame culprit."
  :group 'egg-faces)

(defface egg-blame-subject
  '((((class color) (background light))
     :inherit egg-blame-culprit :foreground "black")
    (((class color) (background dark))
     :inherit egg-blame-culprit :foreground "white")
    (t :inherit egg-blame))
  "Face for blame tag line."
  :group 'egg-faces)

;; (defface egg-log-HEAD
;;   '((t (:inherit region)))
;;   "Face to highlight HEAD in the log buffer."
;;   :group 'egg-faces)

(defface egg-log-HEAD-name
  '((((class color) (background light))
     (:box (:line-width 1 :color "black" :style nil)
	   :inherit egg-branch-mono))
    (((class color) (background dark))
     (:box (:line-width 1 :color "white" :style nil)
	   :inherit egg-branch-mono))
    (t (:box (:line-width 1 :color "yellow" :style nil)
	:inherit egg-branch-mono)))
  "Face to highlight HEAD in the log buffer."
  :group 'egg-faces)

(defcustom egg-buffer-hide-sub-blocks-on-start nil
  "Initially hide all sub-blocks."
  :group 'egg
  :type '(set (const :tag "Status Buffer"   egg-status-buffer-mode)
              (const :tag "Log Buffer"      egg-log-buffer-mode)
              (const :tag "File Log Buffer" egg-file-log-buffer-mode)
              (const :tag "Diff Buffer"     egg-diff-buffer-mode)
              (const :tag "Commit Buffer"   egg-commit-buffer-mode)))

(defcustom egg-buffer-hide-section-type-on-start '((egg-status-buffer-mode . :diff))
  "Initially hide sections of the selected type."
  :group 'egg
  :type '(set (cons :tag "Status Buffer"
                    (const :tag "Hide Blocks of type"
                           egg-status-buffer-mode)
                    (radio (const :tag "Section" :section)
                           (const :tag "File" :diff)
                           (const :tag "Hunk" :hunk)))
	      (cons :tag "Log Buffer"
                    (const :tag "Hide Blocks of type"
                           egg-log-buffer-mode)
                    (radio (const :tag "File" :diff)
                           (const :tag "Hunk" :hunk)))
              (cons :tag "Commit Log Buffer"
                    (const :tag "Hide Blocks of type"
                           egg-commit-buffer-mode)
                    (radio (const :tag "Section" :section)
                           (const :tag "File" :diff)
                           (const :tag "Hunk" :hunk)))
              (cons :tag "Diff Buffer"
                    (const :tag "Hide Blocks of type"
                           egg-diff-buffer-mode)
                    (radio (const :tag "File" :diff)
                           (const :tag "Hunk" :hunk)))))

(defcustom egg-buffer-hide-help-on-start nil
  "Initially hide keybindings help."
  :group 'egg
  :type '(set (const :tag "Status Buffer"   egg-status-buffer-mode)
              (const :tag "Log Buffer"      egg-log-buffer-mode)
              (const :tag "File Log Buffer" egg-file-log-buffer-mode)
              (const :tag "Diff Buffer"     egg-diff-buffer-mode)
              (const :tag "Commit Buffer"   egg-commit-buffer-mode)))

(defcustom egg-log-HEAD-max-len 1000
  "Maximum number of entries when showing the history of HEAD."
  :group 'egg
  :type 'integer)

(defcustom egg-log-all-max-len 3000
  "Maximum number of entries when showing the history of HEAD."
  :group 'egg
  :type 'integer)

(defcustom egg-max-reflogs 10
  "Maximum number of reflogs displayed in the log buffer."
  :group 'egg
  :type 'integer)

(defcustom egg-confirm-next-action t
  "Always prompt for confirmation while guessing the next logical action ."
  :group 'egg
  :type 'boolean)

(defcustom egg-confirm-undo 'show
  "How to ask for confirmation before discarding unstaged chanages."
  :group 'egg
  :type '(radio :tag "Confirmation before (un)applying Hunk"
		(const :tag "Ask" prompt)
		(const :tag "Show and Ask" show)
		(const :tag "Just do It!" nil)))

(defcustom egg-confirm-staging 'show
  "How to ask for confirmation before (un)staging hunk"
  :group 'egg
  :type '(radio :tag "Confirmation before (un)applying Hunk"
		(const :tag "Show and Ask" show)
		(const :tag "Just do It!" nil)))

(defcustom egg-status-buffer-sections '(repo unstaged staged untracked stash)
  "Sections to be listed in the status buffer and their order."
  :group 'egg
  :type '(repeat (choice (const :tag "Repository Info" repo)
                         (const :tag "Unstaged Changes Section" unstaged)
                         (const :tag "Staged Changes Section" staged)
                         (const :tag "Untracked/Unignored Files" untracked)
			 (const :tag "Stashed Work-in-Progess" stash))))


(defcustom egg-commit-buffer-sections '(staged unstaged untracked)
  "Sections to be listed in the status buffer and their order."
  :group 'egg
  :type '(repeat (choice (const :tag "Unstaged Changes Section" unstaged)
                         (const :tag "Staged Changes Section" staged)
                         (const :tag "Untracked/Unignored Files" untracked))))


(defcustom egg-refresh-index-in-backround nil
  "Whether to refresh the index in the background when emacs is idle."
  :group 'egg
  :type 'boolean)

(defcustom egg-enable-tooltip nil
  "Whether to activate useful tooltips, showing the local keymap at the point."
  :group 'egg
  :type 'boolean)

(defcustom egg-git-rebase-subdir "rebase-merge"
  "Name of the rebase's workdir.
Different versions of git have different names for this subdir."
  :group 'egg
  :type '(choice (const ".dotest-merge")
                 (const "rebase-merge")
                 string))

(defcustom egg-show-key-help-in-buffers
  '(:log :status :diff :query :stash)
  "Display keybinding help in egg special buffers."
  :group 'egg
  :type '(set (const :tag "Status Buffer"   :status)
              (const :tag "Log Buffer"      :log)
              (const :tag "Diff Buffer"     :diff)
              (const :tag "Commit Buffer"   :commit)
	      (const :tag "Search Buffer"   :query)
              (const :tag "Stash Buffer"    :stash)))

(define-widget 'egg-quit-window-actions-set 'lazy
  "Custom Type for quit-window actions."
  :offset 4
  :format "%v"
  :type '(set :tag "Actions"
              (const :tag "Kill Buffer" kill)
              (const :tag "Restore Windows" restore-windows)))

(defcustom egg-quit-window-actions nil
  "Actions to perform upon quitting an egg special buffer."
  :group 'egg
  :type '(set (cons :format "%v" (const :tag "Status Buffer" egg-status-buffer-mode)
                    egg-quit-window-actions-set)
              (cons :format "%v"  (const :tag "Log (History) Buffer" egg-log-buffer-mode)
                    egg-quit-window-actions-set)
              (cons :format "%v"  (const :tag "Commit Log Buffer" egg-commit-buffer-mode)
                    egg-quit-window-actions-set)
              (cons :format "%v"  (const :tag "Diff Buffer" egg-diff-buffer-mode)
                    egg-quit-window-actions-set)
              (cons :format "%v"  (const :tag "File Log (History) Buffer" egg-file-log-buffer-mode)
                    egg-quit-window-actions-set)))


(defcustom egg-git-command "git"
  "Name or full-path to the git command.
Set this to the appropriate string in the case where `git' is not the
desirable way to invoke GIT."
  :group 'egg
  :type 'string)

(defcustom egg-cmd-select-special-buffer nil
  "If true, then select the special window invoked by the command.
Instead of just displaying it. Commands like egg-status, unless prefixed
with C-u, only display the special buffer but not selecting it. When
this option is true, invert the meaning of the prefix. I.e. the command
will select the window unless prefixed with C-u."
  :group 'egg
  :type 'boolean)

(defcustom egg-git-diff-options '("--patience")
  "Extra options for git diff."
  :group 'egg
  :type '(set :tag "Extra Diff Options"
	      (radio :tag "Algorithm"
		     (const :tag "Patience" "--patience")
		     (const :tag "Historgram" "--histogram")
		     (const :tag "Minimal" "--minimal"))
	      (radio :tag "White Space (Warning: might cause problem for hunk (un)staging)"
		      (const :tag "Ignore Space at End-of-Line" 
			     "--ignore-space-at-eol")
		      (const :tag "Ignore Space Changes"
			     "--ignore-space-change")
		      (const :tag "Ignore All Space"
			     "--ignore-all-space"))))

(defcustom egg-git-diff-file-options-alist 
  '((c-mode "--patience" "--ignore-all-space")
    (emacs-lisp-mode "--patience" "--ignore-all-space")
    (text-mode "--histogram")
    (makefile-mode "--patience" "--ignore-space-at-eol"))
  "Extra options for when show diff of a file matching a major mode."
  :group 'egg
  :type '(repeat (cons :tag "File Diff"
		       (choice :tag "Mode"
			       (const :tag "C" c-mode)
			       (const :tag "C++" c++-mode)
			       (const :tag "Java" java-mode)
			       (const :tag "Text" text-mode)
			       (const :tag "ELisp" emacs-lisp-mode)
			       (const :tag "Lisp" lisp-mode)
			       (const :tag "Makefile" makefile-mode)
			       (const :tag "Python" python-mode)
			       (const :tag "Perl" perl-mode)
			       (symbol :tag "Other"))
		       (set :tag "Extra Options"
			    (radio :tag "Algorithm"
				    (const :tag "Patience" "--patience")
				    (const :tag "Historgram" "--histogram")
				    (const :tag "Minimal" "--minimal"))
			    (radio :tag "White Space"
				    (const :tag "Ignore Space at End-of-Line" 
					   "--ignore-space-at-eol")
				    (const :tag "Ignore Space Changes"
					   "--ignore-space-change")
				    (const :tag "Ignore All Space"
					   "--ignore-all-space"))))
		 ))

(defcustom egg-git-merge-strategy-options nil
  "Merge options"
  :group 'egg
  :type 
  '(choice :tag "Strategy"
	   (const :tag "Merge uses Git's Default" nil)
	   (cons :tag "Recursive" 
		 (const :tag "Merge uses Recursive Strategy" "--strategy=recursive")
		 (set :tag "Recursive Options"
		      (radio :tag "Favour 1 side"
			     (const :tag "Favour Their Side" "--strategy-option=theirs")
			     (const :tag "Favour Our Side" "--strategy-option=ours"))
		      (const :tag "Be Patience" "--strategy-option=patience")
		      (radio :tag "Ignore Space"
			     (const :tag "Ignore Space Change at EOL"
				    "--strategy-option=ignore-space-at-eol")
			     (const :tag "Ignore Space Change"
				    "--strategy-option=ignore-space-change")
			     (const :tag "Ignore All Space"
				    "--strategy-option=ignore-all-space"))))
	   (list :tag "Resolve" (const :tag "Merge uses Resolve Strategy" 
				       "--strategy=resolve"))
	   (list :tag "Octopus" (const :tag "Merge uses Octopus Strategy" 
				       "--strategy=octopus"))
	   (list :tag "Ours" (const :tag "Merge just keeps Our Side" 
				       "--strategy=octopus"))
	   ))

(defcustom egg-log-buffer-marks "+~.*_@>"
  "A vector of 4 characters used for marking commit in the log buffer.
The first 3 elemements are used to mark a commit for the upcoming interactive rebase.
The 1st element is used to mark a commit to be picked, the 2nd to be edited and
the 3rd to be squashed. The 4th element is used to mark a commit as the BASE
commit."
  :group 'egg
  :type '(radio :tag "Commit Marking Characters" 
		(const :tag "+ ~ . * >" "+~.*>")
		(const :tag "✔ ✎ ↶ ➤ ⚒" "✔✎↶➤⚒")
		(const :tag "✔ ✎ ↯ ➤ ⚒" "✔✎↯➤⚒")
		(const :tag "✔ ✎ ▼ ● ⚒" "✔✎▼●⚒")
		(const :tag "✔ ✎ ↯ ● ⚒" "✔✎↯●⚒")
		(const :tag "✔ ✍ ↶ ➤ ⚒" "✔✍↶➤⚒")
		(const :tag "✔ ✍ ↯ ➤ ⚒" "✔✍↯➤⚒")
		(vector :tag "Pick Individual Character"
			(radio :tag "Pick Mark"
			       (const :tag "+" ?+)
			       (const :tag "✔" #x2714)
			       (const :tag "♥" #x2665)
			       (const :tag "▶" #x24b6)
			       character)
			(radio :tag "Edit Mark"
			       (const :tag "~" ?~)
			       (const :tag "✍" #x270d)
			       (const :tag "✎" #x270e)
			       (const :tag "✒" #x2712)
			       character)
			(radio :tag "Squash Mark"
			       (const :tag "." ?.)
			       (const :tag "↶" #x21b6)
			       (const :tag "↯" #x21af)
			       (const :tag "┅" #x2505)
			       (const :tag "♻" #x267b)
			       character)
			(radio :tag "Base Mark"
			       (const :tag "*" ?*)
			       (const :tag "★" #x2605)
			       (const :tag "█" #x2588)
			       (const :tag "➤" #x27a4)
			       (const :tag "■" #x27a0)
			       (const :tag "▣" #x27a3)
			       (const :tag "●" #x25cf)
			       (const :tag "✽" #x273d)
			       (const :tag "☻" #x263b)
			       character)
			(radio :tag "Fixup Mark"
			       (const :tag ">" ?>)
			       (const :tag "⚒" #x2692)
			       (const :tag "┅" #x2505)
			       (const :tag "♻" #x267b)
			       character))
		string))

(defsubst egg-log-buffer-pick-mark () (aref egg-log-buffer-marks 0))
(defsubst egg-log-buffer-edit-mark () (aref egg-log-buffer-marks 1))
(defsubst egg-log-buffer-squash-mark () (aref egg-log-buffer-marks 2))
(defsubst egg-log-buffer-base-mark () (aref egg-log-buffer-marks 3))
(defsubst egg-log-buffer-fixup-mark () (aref egg-log-buffer-marks 4))

(defcustom egg-commit-file-select-mark ?+
  "Character used to mark a commit's file to be used in the upcoming cherry picking."
  :group 'egg
  :type '(radio :tag "File Select Mark"
		 (const :tag "✔" #x2714)
		 (const :tag "⬅" #x2b05)
		 (const :tag "◀" #x25c0)
		 (const :tag "▶" #x25b6)
		 (const :tag "➤" #x27a4)
		 (const :tag "★" #x2605)
		 (const :tag "●" #x25cf)
		 (const :tag "⬤" #x2b24)
		 (const :tag "+" ?+)
		 character))

(defcustom egg-log-graph-chars "*|-/\\"
  "Characters used to re-draw the git-log's graph in the log buffer.
Using custom characters will slow down the log buffer rendering.
On slow machine, stick to the default which is not changing the git's drawing.
The 1st char is the bullet for the node in the graph.
The 2nd char is the veritcal line in the graph.
The 3rd char is the dash between the graph and the sha1.
The 4th char is the diagonal line from lower-left to upper right.
The 5th char is the diagonal line from upper-lef to lower-right."
  :group 'egg
  :type '(radio :tag "Log Graph Characters"
		(const :tag "* | - / \\" "*|-/\\")
		(const :tag "● │ ─ ╱ ╲" "●│─╱╲")
		(const :tag "█ │ ─ ╱ ╲" "█│─╱╲")
		(vector :tag "Pick individual character"
			(radio :tag "Node"
			       (const :tag "*" ?*)
			       (const :tag "◉" #x25c9)
			       (const :tag "○" #x25cb)
			       (const :tag "●" #x25cf)
			       (const :tag "■" #x25a0)
			       (const :tag "□" #x25a1)
			       (const :tag "█" #x2588))
			(radio :tag "Vertical"
			       (const :tag "|" ?|)
			       (const :tag "│" ?│))
			(radio :tag "Horizontal"
			       (const :tag "-" ?-)
			       (const :tag "─" ?─))
			(radio :tag "Lower-Left to Upper-Right"
			       (const :tag "/" ?/)
			       (const :tag "╱" ?╱))
			(radio :tag "Upper-Left to Lower-Right"
			       (const :tag "\\" ?\\)
			       (const :tag "╲" ?╲))
			)))


(provide 'egg-custom)
