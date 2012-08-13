;;;
;;; alpaca.el -- an easy way to edit GnuPG files encrypted with 
;;;              shared-key cryptography
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct 16, 2003
;; Revised: Feb 17, 2008

;;; Commentary:

;;
;; Use Emacs 20.7 or later and GnuPG ver 1.2.3 or later.
;;
;; 0) Put the following to your ".emacs"
;;
;;	(autoload 'alpaca-after-find-file "alpaca" nil t)
;;	(add-hook 'find-file-hooks 'alpaca-after-find-file)
;;
;; 1) To load GnuPG file, say foo.gpg
;;
;;	C-xC-f + foo.gpg
;;	And type its passphrase.
;;
;; 2) To save the buffer to the file
;;
;;	C-xC-s
;;	If 'alpaca-cache-passphrase' is non-nil, the passpharse used 
;;	on decryption is automatically used to encrypt the buffer.
;;
;; Note that your passphrase is never saved to a file. It exists only 
;; in the memory. However, plain text is stored to a temporary file both 
;; on encryption and decryption. The temporary file is created in the
;; same directory of the GnuPG file. You should not use a network file
;; system for the directory. Note also that creating temporary files
;; potentially causes race condition on Emacs 20.7.
;;
;; Note that the old name of this file is "gpg.el". To avoid conflict
;; with other Elisp files, this file was renamed to "alpaca.el".
;; "Alpaca" is an animal name, not an acronym for any words.
;;

;;; Code:

(defconst alpaca-version "0.13")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable variables
;;;

(defvar alpaca-map nil)

(unless alpaca-map
  (setq alpaca-map (make-sparse-keymap))
  (define-key alpaca-map "\C-x\C-s" 'alpaca-save-buffer))

(defvar alpaca-program "gpg")

(defvar alpaca-cipher "AES"
  "*String name of shared-key cryptography. 
To know names of supported algorithms, type \"gpg --version\".")

(defvar alpaca-cache-passphrase t
  "*If non-nil, the passpharse used on decryption is used
to encrypt the buffer when saving.")

(defvar alpaca-regex-suffix "\\.gpg$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal variables
;;;

(defvar alpaca-rendezvous nil)
(defvar alpaca-passphrase nil)

(mapcar 'make-variable-buffer-local
	(list 'alpaca-rendezvous
	      'alpaca-passphrase))

(defvar alpaca-process-encryption "*gpg encryption*")
(defvar alpaca-process-decryption "*gpg decryption*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub-functions
;;;

(defun alpaca-file-modified-time (file)
  (nth 5 (file-attributes file)))

(defun alpaca-file-newer (t1 t2)
  (and t1
       (or (null t2) 
	   (> (nth 0 t1) (nth 0 t2))
	   (and (= (nth 0 t1) (nth 0 t2))
		(> (nth 1 t1) (nth 1 t2))))))

(defalias 'alpaca-make-temp-file
  (if (fboundp 'make-temp-file) 'make-temp-file 'make-temp-name))

(defun alpaca-start-process (name buffer program &rest program-args)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "LANGUAGE" "C")
    (setenv "LC_ALL" "C")
    (setenv "LANG" "C")
    (apply 'start-process name buffer program program-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decrypting and loading
;;;

(defun alpaca-which (file path)
  (catch 'loop
    (while path
      (if (file-exists-p (expand-file-name file (car path)))
	  (throw 'loop t)
	(setq path (cdr path))))))

(defun alpaca-buffer-hack ()
  (let ((pass alpaca-passphrase) ;; major mode kills local variables
	(buffer-file-name (file-name-sans-extension buffer-file-name)))
    (set-auto-mode)
    (hack-local-variables)
    ;; Since save-buffer() is not used, we don't have to take care of
    ;; make-backup-files
    (auto-save-mode -1)
    (alpaca-setup-keymap)
    (setq alpaca-passphrase pass)))

(defun alpaca-after-find-file ()
  (when (string-match alpaca-regex-suffix (buffer-file-name))
    (if (alpaca-which alpaca-program exec-path)
	(if (= (buffer-size) 0)
	    (progn
	      (set-auto-mode)
	      (alpaca-buffer-hack))
	  (alpaca-insert-file-contents))
      (kill-buffer (current-buffer))
      (message "\"%s\" does not exist" alpaca-program))))

(defun alpaca-insert-file-contents ()
  "Erases the current buffer, decrypting the corresponding file
and inserts it.
If 'alpaca-cache-passphrase' is non-nil,
the passphrase used on decryption is stored as a local variable.
To save the buffer and encrypt the file, type \\<alpaca-map>\\[alpaca-save-buffer].
See also 'alpaca-save-buffer'."
  (interactive)
  (let* ((process-connection-type t) ;; 'pty
	 (buf (current-buffer))
	 (file (buffer-file-name))
	 (tfile (alpaca-make-temp-file file))
	 (oldt (alpaca-file-modified-time tfile))
	 newt pro)
    (unwind-protect
	(progn
	 (setq pro (alpaca-start-process alpaca-process-decryption buf alpaca-program
					 "-d" "--yes" "--output" tfile file))
	 (set-process-filter   pro 'alpaca-filter)
	 (set-process-sentinel pro 'alpaca-sentinel)
	 (setq alpaca-rendezvous t)
	 (while alpaca-rendezvous
	   (sit-for 0.1)
	   (discard-input))
	 (setq newt (alpaca-file-modified-time tfile))
	 (cond
	  ((alpaca-file-newer newt oldt)
	   (erase-buffer)
	   (if (default-value 'enable-multibyte-characters)
	       (set-buffer-multibyte t))
	   (let ((coding-system-for-read 'undecided))
	     (insert-file-contents tfile))
	   (alpaca-buffer-hack)
	   (set-buffer-file-coding-system last-coding-system-used)
	   (setq buffer-undo-list nil)
	   (set-buffer-modified-p nil))
	  (t
	   (switch-to-buffer (car (buffer-list)))
	   (kill-buffer buf)))
	 (set (make-variable-buffer-local 'alpaca-p) t))
      (alpaca-delete-file tfile))))

(defun alpaca-setup-keymap ()
  "Set \\C-x\\C-s to 'alpaca-save-buffer."
  (interactive)
  (let ((map (copy-keymap alpaca-map)))
    (set-keymap-parent map (current-local-map))
    (use-local-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Saving and encrypting
;;;

(defun alpaca-save-buffer ()
  "Saves the buffer and encrypts the file. 
If the passphrase is cached in the buffer, it is used on encryption
automatically. Otherwise, type your passphrase twice.
See also 'alpaca-find-file'."
  (interactive)
  (if (not (buffer-modified-p))
      (message "(No changes need to be saved)")
    (let* ((file (buffer-file-name))
	   (oldt (alpaca-file-modified-time file))
	   (tfile (alpaca-make-temp-file file))
	   (buf (current-buffer))
	   (process-connection-type t) ;; 'pty
	   pro newt)
      (unwind-protect
	  (progn
	    (write-region (point-min) (point-max) tfile nil 'no-msg)
	    (setq pro (alpaca-start-process alpaca-process-encryption buf alpaca-program
					 "-c" "--cipher-algo" alpaca-cipher
					 "--yes" "--output" file tfile))
	    (set-process-filter   pro 'alpaca-filter)
	    (set-process-sentinel pro 'alpaca-sentinel)
	    (setq alpaca-rendezvous t)
	    (while alpaca-rendezvous
	      (sit-for 0.1)
	      (discard-input))
	    (setq newt (alpaca-file-modified-time file))
	    (if (not newt)
		(debug)
	      (when (alpaca-file-newer newt oldt)
		(set-buffer-modified-p nil)
		(set-visited-file-modtime)
		(message (format "Wrote %s with GnuPG" file)))))
	(alpaca-delete-file tfile)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun alpaca-read-passwd (prompt &optional encrypt-p)
  (if (and alpaca-cache-passphrase alpaca-passphrase)
      (progn
	(sit-for 0.01) ;; Emacs 20.7 
	alpaca-passphrase)
    (let ((pass (read-passwd prompt)))
      (if (and alpaca-cache-passphrase (not encrypt-p))
	  (setq alpaca-passphrase pass))
      pass)))

(defun alpaca-debug (string)
  (save-excursion
    (set-buffer (get-buffer-create "*Alpaca Debug*"))
    (goto-char (point-max))
    (insert (format "%s\n" string))))

(defun alpaca-filter (process string)
  (let* ((name (process-name process))
	 (regex (concat "^" (regexp-quote alpaca-process-encryption)))
	 (encrypt-p (string-match regex name))
	 (buf (process-buffer process)))
    (when (get-buffer buf)
      (save-excursion
	(set-buffer buf)
	(cond
	 ((string-match "invalid passphrase" string)
	  (message "Passphrase mismatch!")
	  (setq alpaca-passphrase nil))
	 ((string-match "bad key" string)
	  (message "Passphrase is wrong!")
	  (setq alpaca-passphrase nil))
	 ((string-match "Enter passphrase:" string)
	  (process-send-string process (alpaca-read-passwd "Passphrase: " encrypt-p))
	  (process-send-string process "\n"))
	 ((string-match "Repeat passphrase:" string)
	  (process-send-string process (alpaca-read-passwd "Passphrase again: "))
	  (process-send-string process "\n"))
	 ((string-match "exiting" string)
	  (setq alpaca-rendezvous nil)))))))

(defun alpaca-sentinel (process event)
  (let ((buf (process-buffer process)))
    (when (get-buffer buf)
      (set-buffer buf)
      (setq alpaca-rendezvous nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Common functions
;;;

(defun alpaca-kill-buffer ()
  (when (and (boundp 'alpaca-p) 
	     (eq alpaca-p t)
	     (buffer-modified-p))
    (alpaca-save-buffer)))

(add-hook 'kill-buffer-hook 'alpaca-kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Common functions
;;;

(defun alpaca-delete-file (file)
  (when (file-exists-p file)
    (with-temp-buffer
      (let ((coding-system-for-write 'binary)
	    (size (nth 7 (file-attributes file)))
	    (i 0))
	(while (< i size)
	  (insert 0)
	  (setq i (1+ i)))
	(write-region (point-min) (point-max) file nil 'no-msg)))
    (delete-file file)))

(provide 'alpaca)

;;; Copyright Notice:

;; Copyright (C) 2003-2006 Kazu Yamamoto
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the author nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; alpaca.el ends here
