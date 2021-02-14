;;; bostr.el --- Backup On Save of a file To RCS

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Benjamin Rutt <brutt@bloomington.in.us>
;; Maintainer: Conor Nash <conor@nashcobusinessservicesllc.com>
;; Version: 1.4

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Ever wish to go back to an older saved version of a file?  Then
;; this package is for you.  This package copies every file you save
;; in Emacs to a backup directory tree (which mirrors the tree
;; structure of the filesystem), as an RCS revision file.  Never lose
;; old saved versions again.

;; To activate globally, place this file in your `load-path', and add
;; the following lines to your ~/.emacs file:
;;
;; (require 'bostr)
;; (add-hook 'after-save-hook 'bostr)

;; To activate only for individual files, add the require line as
;; above to your ~/.emacs, and place a local variables entry at the
;; end of your file containing the statement:
;;
;; eval: (add-hook (make-local-variable 'after-save-hook) 'bostr)
;;
;; NOTE:  I would give a full example of how to do this here, but it
;; would then try to activate it for this file since it is a short
;; file and the docs would then be within the "end of the file" local
;; variables region.  :)

;; To filter out which files it backs up, use a custom function for
;; `bostr-filter-function'.  For example, to filter out the saving of
;; gnus .newsrc.eld files, do:
;;
;; (defun bostr-no-newsrc-eld (filename)
;;   (cond
;;    ((string= (file-name-nondirectory filename) ".newsrc.eld") nil)
;;    (t t)))
;; (setq bostr-filter-function 'bostr-no-newsrc-eld)

;;; Notes:
;;; Code:

(defgroup bostr nil
  "Backup On Save To RCS."
  :version "28.0")

(defcustom bostr-mirror-location "~/.backups-rcs"
  "Directory for mirror tree of RCS directories."
  :group 'bostr)

(defcustom bostr-remote-files nil
  "Whether to backup remote files at each save.

Defaults to nil."
  :group 'bostr)

(defcustom bostr-filter-function #'identity
  "Function which should return non-nil if the file should be backed up."
  :group 'bostr)

(defcustom bostr-size-limit 50000
  "Maximum size of a file (in bytes) that should be copied at each savepoint.
If a file is greater than this size, don't make a backup of it.
Setting this variable to nil disables backup suppressions based
on size."
  :group 'bostr)

(defcustom bostr-rcs "/usr/bin/rcs"
  "Path to the rcs executable."
  :group 'bostr)

(defcustom bostr-ci "/usr/bin/ci"
  "Path to the ci executable."
  :group 'bostr)


;;;###autoload
(defun bostr ()
  (let ((bfn (buffer-file-name)))
    (when (and (or bostr-remote-files
		   (not (file-remote-p bfn)))
	       (funcall bostr-filter-function bfn)
	       (or (not bostr-size-limit)
		   (<= (buffer-size) bostr-size-limit)))
      (let ((backup-path (bostr-compute-location bfn))
            (log (get-buffer-create "*Bostr-log*")))
        (copy-file bfn backup-path t t t)
        (call-process bostr-ci nil log nil "-f" "-m''" "-t-''" backup-path)
        (call-process bostr-rcs nil log nil "-U" backup-path)))))

(defun bostr-compute-location (filename)
  (let* ((containing-dir (file-name-directory filename))
	 (basename (file-name-nondirectory filename))
	 (backup-container
	  (format "%s%s"
		  (expand-file-name bostr-mirror-location)
		  containing-dir)))
    (when (not (file-exists-p backup-container))
      (make-directory (concat backup-container "/RCS") t))
    (format "%s%s" backup-container basename)))

(provide 'bostr)
;;; bostr.el ends here
