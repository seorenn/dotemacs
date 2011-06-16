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
;; This script provide `postform-markup' function only.
;;
;; Select region, then command `M-x' `postform-markup'
;; This command will convert selected text to HTML Style format
;;
;; This is not Markdown Compiler.
;; `postform-markup' was not process line-break or paragraph.
;;
;; ...
;;
;; 아악 귀찮아서 한글로 할래!
;;
;; 이 파일은 postform-markup 함수를 제공합니다.
;; 이 함수는 그냥 markdown 문법의 일부 만 적용하여 HTML화 시켜버립니다.
;; 영역을 선택한 후 M-x postform-markup 커맨드를 실행시키는 것으로 사용법은 끝입니다.
;;
;; 이걸 만든 이유는...
;; 블로깅 할 내용을 한 파일에 여러개 적는게 제 스타일인데
;; 이걸 markdown으로 컴파일 해 버리니 뭔가 좀 복잡하고
;; 특히 제가 블로깅 하는 blogger(blogspot.com)의 글쓰기 옵션과
;; markdown으로 컴파일한 내용이 모양이 맞지 않기 때문입니다.
;;
;; 즉, 블로거 안쓰시는 분은 거의 쓸 모 없는 함수입니다. ㅋㅋㅋ

(if (not (fboundp 'replace-in-string))
    (defun replace-in-string (string regexp replacement &optional literal)
      "Replace regex in string with replacement"
      (replace-regexp-in-string regexp replacement string t literal)))

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
    (setq tmpline (replace-regexp-in-string "`\\([^`]+\\)`" "<code>\\1</code>" tmpline t))
    (setq tmpline (replace-regexp-in-string "\\*\\([^*]+\\)\\*" "<bold>\\1</bold>" tmpline t))
    tmpline))

(defun postform-markup-heading (line)
  "Mark-up heading syntax"
  (let ((tmpline line)
        (hstr nil)
        (hlevel 0)
        (fmtstr nil))
    (setq hstr (replace-regexp-in-string "\\(^#+ \\).+" "\\1" tmpline t))
    (when hstr
      (progn
        (setq hlevel (length hstr))
        (when (> hlevel 1)
          (progn
            (setq hlevel (1- hlevel))
            (setq fmtstr
                  (format "<h%d>\\2</h%d>" hlevel hlevel))
            (setq tmpline (replace-regexp-in-string "\\(^#+ \\)\\(.+\\)" fmtstr tmpline t))))
        ))
    tmpline
    ))

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
   (
    (or
    (string= (substring line 0 2) "* ")
    (string= (substring line 0 2) "- ")
    )
    t)
   (t nil)))

(defun postform-markup-line (line in-pre)
  "Mark-up line"
  (progn
    (cond
     ((not in-pre)
      (unless (postform-pre-end? line)
        (progn
          (setq line (postform-escape-html line))
          (if (postform-ul? line)
            (setq line (postform-markup-ul-item line))
            (setq line (postform-markup-heading line)))
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
       ((and in-ul (not (postform-ul? line)))
        (setq in-ul nil)
        (setq append-ul-end t)))
      (setq line (postform-markup-line line in-pre))
      (when append-ul-end
        (progn
          (setq output (append output (list "</ul>")))
          (setq append-ul-end nil)))
      (setq output (append output (list line)))
      )
    output))
      
(defun postform-markup ()
  "Convert Posting-Form from text selected region by Markdown-similary syntax"
  (interactive)
  (let (regbuf
        lines)
    (if (and transient-mark-mode mark-active)
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
      (insert regbuf))
    (message "Select region first!"))))

(provide 'postform-markup)