;;; bostr.el --- Backup On Save To RCS

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Benjamin Rutt <brutt@bloomington.in.us>
;; Maintainer: Conor Nash <conor@nashcobusinessservicesllc.com>
;; Maintainer: John S. Yates, Jr. <john@yates-sheets.org>
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

;; Derived from and then heavily modified:
;;   https://www.emacswiki.org/emacs/backup-each-save.el
;;
;; Modern version control system, such as git, are wonderful.  But they
;; have draw backs when dealing with lightweight saves:
;;
;; * Too invasive: new revisions created only by explicitly action
;; * Too coarse: a revision captures an entire "project"
;; * Requires setup: what about files that have no project?
;; * Files listed in .gitignore (or equivalent) may still get edited
;;
;; Enter bostr.  EVERY time you save a file bostr:
;;
;; * Makes a readonly mirror of that saved file in the mirror area
;; * Ensures presence of a sibling RCS directory
;; * Records this newest file version as the latest RCS revision
;;
;; To activate globally, place this file in your `load-path', and add
;; the following lines to your ~/.emacs file:
;;
;; (require 'bostr)
;; (add-hook 'after-save-hook 'bostr)
;;
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
  :group 'backup
  :version "28.0")

(defcustom bostr-mirror-location "~/.backups-rcs"
  "Directory for mirror tree of RCS directories (no trailing '/')."
  :group 'bostr
  :type 'directory)

(defcustom bostr-remote-files nil
  "Whether to backup remote files at each save (off by default)."
  :type 'boolean
  :group 'bostr)

(defcustom bostr-filter-function #'identity
  "Function which should return non-nil if the file should be backed up."
  :type 'function
  :group 'bostr)

(defcustom bostr-size-limit 50000
  "Maximum size of a file (in bytes) that should be copied at each savepoint.
If a file is greater than this size, don't make a backup of it.
Setting this variable to nil disables backup suppressions based
on size."
  :type 'natnum
  :group 'bostr)

(defcustom bostr-rcs "/usr/bin/rcs"
  "Path to the rcs executable."
  :type '(file :must-match t)
  :group 'bostr)

(defcustom bostr-ci "/usr/bin/ci"
  "Path to the ci executable."
  :type '(file :must-match t)
  :group 'bostr)

(defconst bostr-witnesses-regex
  "/\\(SCCS\\|RCS\\|CVS\\|MCVS\\|[.]src\\|[.]svn\\|[.]git\\|[.]hg\\|[.]bzr\\|_MTN\\|_darcs\\|[{]arch[}]\\)/"
  "Writes to any point below one of these witnesses should be ignored.

FIXME: Duplicated from vc-directory-exclusion-list.")

;;;###autoload
(defun bostr ()
  "Mirror buffer's file and add a new RCS revision"
  (setq vc-consult-headers nil)
  (let ((bfn (buffer-file-name)))
    (when (and (not (string-match bostr-witnesses-regex bfn))
               (or bostr-remote-files
		   (not (file-remote-p bfn)))
	       (funcall bostr-filter-function bfn)
	       (or (not bostr-size-limit)
		   (<= (buffer-size) bostr-size-limit)))
      (let* ((dir (file-name-directory bfn))
	     (file (file-name-nondirectory bfn))
	     (mirror-dir (concat (expand-file-name bostr-mirror-location) dir))
             (mirror-file (concat mirror-dir file))
             (rcs-dir (concat mirror-dir "RCS/"))
             (rcs-file (concat rcs-dir file ",v"))
             (log (get-buffer-create "*Bostr-log*")))
        (when (not (file-exists-p rcs-dir))
          (make-directory rcs-dir t))
        (when (file-exists-p mirror-file)
          (delete-file mirror-file))
        (copy-file bfn mirror-file t t t)
        (call-process bostr-ci nil log nil "-l" "-m''" "-t-''" mirror-file)
        (set-file-modes mirror-file #o444)))))

(provide 'bostr)

;;; bostr.el ends here
