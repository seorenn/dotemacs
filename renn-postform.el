;;; renn-postform.el - Simple mark-up function similary Markdown

;; Copyright (C) 2011 Seorenn

;; Author: Seorenn <hirenn@gmail.com>
;; Maintainer: Seorenn <hirenn@gmail.com>
;; Created: 2011-04-28
;; Version: 0.1
;; Keywords: blogger, blogspot, markdown

;;; This file is not part of GNU Emacs.

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary
;;
;; Select region, then command `M-x postform-markup'
;; Selected text will be converted to HTML format

(defun postform-escape-html (line)
  "Escape HTML Special Characters in line"
  (let ((tmpline line))
    (setq tmpline (replace-in-string tmpline "&" "&amp;"))
    (setq tmpline (replace-in-string tmpline "<" "&lt;"))
    (setq tmpline (replace-in-string tmpline ">" "&gt;"))
    tmpline))

(defun postform-markup-text-syntax (line)
  "Mark-up line"
  (let ((tmpline line))
    (setq tmpline (replace-regexp-in-string "`\\(.+\\)`" "<code>\\1</code>" tmpline t))
    (setq tmpline (replace-regexp-in-string "\\*\\(.+\\)\\*" "<bold>\\1</bold>" tmpline t))
    tmpline))

(defun postform-markup-ul-item (line)
  "Mark-up item in UL block"
  (progn
    (setq line (substring line 2))
    (setq line (concat "<li>" line))
    (setq line (concat line "</li>"))
    line))

(defun postform-pre-end? (line)
  "check line equals <pre> or </pre>"
  (if (string-match "</[pR][rR][eE]>" line) t nil))

(defun postform-pre-start? (line)
  (if (string-match "<[pP][rR][eE]>" line) t nil))

(defun postform-ul? (line)
  "check line containts ul syntax"
  (cond
   ((< (length line) 2) nil)
   ((string= (substring line 0 2) "* ") t)
   (t nil)))

(defun postform-markup-line (line in-pre)
  "Mark-up line"
  (progn
    (cond
     ((not in-pre)
      (unless (postform-pre-end? line)
        (progn
          (setq line (postform-escape-html line))
          (when (postform-ul? line)
            (setq line (postform-markup-ul-item line)))
          (setq line (postform-markup-text-syntax line)))))
     (in-pre
      (unless (postform-pre-start? line)
        (progn
          (setq line (postform-escape-html line))))))
    line))

(defun postform-markup-lines (lines)
  "Mark-up list of lines"
  (let (output
        line
        in-pre
        in-ul
        append-ul-end)
    (dolist (line lines)
      (cond
       ((postform-pre-start? line)
        (setq in-pre t)
        (when in-ul
          (setq output (append output (list "</ul>"))))
        (setq in-ul nil))
       ((postform-pre-end? line) (setq in-pre nil))
       ((and (not in-pre) (postform-ul? line))
        (unless in-ul
          (setq output (append output (list "<ul>"))))
        (setq in-ul t))
       ((and in-ul (not postform-ul? line))
        (setq in-ul nil)
        (setq append-ul-end)))
      (setq line (postform-markup-line line in-pre))
      (setq output (append output (list line)))
      (when append-ul-end
        (setq output (append output (list "</ul>")))))
    output))
      
(defun postform-markup ()
  "Convert Posting-Form from text selected region by Markdown-similary syntax"
  (interactive)
  (let (regbuf
        lines)
    (save-excursion
      (setq regbuf (buffer-substring-no-properties
                    (region-beginning) (region-end)))
      ;; mark-up lines
      (setq lines
            (postform-markup-lines
             (split-string regbuf "\n")))
      ;; change selected text to result of mark-up
      (delete-region (region-beginning) (region-end))
      (setq regbuf (mapconcat 'identity lines "\n"))
      (insert regbuf))))

(provide 'postform-markup)