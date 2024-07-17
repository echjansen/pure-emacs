;;; pure-common.el --- Pure Emacs common configuration. -*- lexical-binding: t -*-

;; Copyright (C) 2024 echjansen

;; This file is part of = P U R E - E M A C S =
;;
;; pure-emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; pure-emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with pure-emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This files contains variables and functions that are common to the
;; various pure-emacs modules. Include (require) in each module that
;; makes use of one or more variables.
;; Note that this file is used for personal configuration details and
;; is likely required to be modiifed.

;;; Code:

;;;; Directories
;;;;; Pure Emacs home directory
(defconst pure-dir-emacs (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

;;;;; Pure Emacs modules directory
(defconst pure-dir-modules (concat user-emacs-directory "modules")
  "The path to the emacs.d modules directory.")

;;;;; Pure Emacs cache diretcory (info shared between Emacs instances
(defconst pure-dir-cache "~/.cache/emacs/"
  "The directory for Pure-Emacs littered files.")

;;;;; Pure Emacs backup directory
(defconst pure-dir-backup (concat pure-dir-cache "backup/")
  "The directory for Pure-Emacs backup files.")

;;;;; Pure Emacs private directory
(defconst pure-dir-private "~/.config/emacs/"
  "The directory for Pure-Emacs backup files.")

;;;;; Pure Emacs notes directory
(defconst pure-dir-notes "~/Projects/pure-notes/"
  "The directory for Pure-Emacs note files directory.")

;;;;; Create directories if non existing
(dolist (dir (list pure-dir-cache
                   pure-dir-backup
                   pure-dir-notes
                   pure-dir-private))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;;; Files
;; Note: if you are creating ~secret~ files with the ~.gpg~ extension, then
;; you should create these files manually - as the ~sender~ will be required
;; to encrypt / decrypt that file. Use C-x C-f to create the file, and C-x C-w
;; to write the file
;;;;; Pure Emacs custom file
;; Some variables may contain ~secret~ information
;; The custom file can store these variables securely by saving them as .gpg
(defconst pure-custom-file "~/.config/emacs/pure-custom.el.gpg"
  "The pure-emacs custom file containing secrets")

;;;;; Pue Emacs abbreviations file
(defconst pure-abbrev-defs "~/.config/emacs/pure-abbrev-defs"
  "The pure-emacs abbreviations file")

;;;;; Create files if non existing
(dolist (file (list pure-custom-file
                    pure-abbrev-defs))
  (unless (file-exists-p file)
    (with-temp-file file)))

(provide 'pure-common)
;;; pure-common.el ends here
