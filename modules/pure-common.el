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

;;;;; Pure Emacs private directory
(defconst pure-dir-private (concat user-emacs-directory "data")
  "The directory for Pure-Emacs backup files.")

;;;;; Pure Emacs cache diretcory (info shared between Emacs instances
(defconst pure-dir-cache "~/.cache/emacs/"
  "The directory for Pure-Emacs littered files.")

;;;;; Pure Emacs backup directory
(defconst pure-dir-backup (concat pure-dir-cache "backup/")
  "The directory for Pure-Emacs backup files.")

;;;;; Pure Emacs notes directory
(defconst pure-dir-notes "~/Projects/pure-notes/"
  "The directory for Pure-Emacs note files directory.")

;;;;; Create directories if non existing
(dolist (dir (list pure-dir-cache
                   pure-dir-backup
                   pure-dir-notes))
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
(defconst pure-custom-file "~/.config/emacs/pure-custom.el"
  "The pure-emacs custom file containing secrets")

;;;;; Pure Emacs abbreviations file
(defconst pure-abbrev-defs "~/.config/emacs/pure-abbrev-defs"
  "The pure-emacs abbreviations file")

;;;;; Create files if non existing
(dolist (file (list pure-custom-file
                    pure-abbrev-defs))
  (unless (file-exists-p file)
    (with-temp-file file)))

(provide 'pure-common)
;;;; Package Manager
;;;;; = package.el - package installation

;; Disable package initializes.  We either initialize it
;; anyway in case of interpreted Emacs, or we don't want slow
;; initizlization in case of byte-compiled Emacs.
;; Disabling must be configured in early-init, or package initialization will occur.
;; This is for reference only.
;; (setq package-enable-at-startup nil)

;; Ask package.el to not add (package-initialize) to init.el.
(setq package--init-file-ensured t)

;; Traverse the installed packages and add their paths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        ;; (require 'package)
        (package-initialize)

        ;; Sources for packages (recipes in this case)
        (setq package-archives
              '(("melpa" . "https://melpa.org/packages/")
                ("melpa-stable" . "https://stable.melpa.org/packages/")
                ("gnu" . "https://elpa.gnu.org/packages/")
                ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

        ;; Order of archive priority. The higher the number the higher the priority
        (setq package-archive-priorities
              '(("melpa"         . 4)
                ("gnu"           . 3)
                ("nongnu"        . 2)
                ("melpa-stable"  . 1)))

        ;; use-package configuration during compilation
        (require 'use-package)
        (setq use-package-always-ensure t)
        (setq use-package-always-defer t)
        (setq use-package-compute-statistics nil)
        (setq use-package-expand-minimally t)

        ;; package configuration during compilation
        (setq package-install-upgrade-built-in nil)

        ;; hard-code load-path in compiled file
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p
                                            package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

;; Info of packages is managed via autoloads. Since autoloads are not in use
;; this code parses the package folder and adds folders with info to the list
(with-eval-after-load "info"
  (info-initialize)
  (dolist (dir (directory-files package-user-dir))
    (let ((fdir (concat (file-name-as-directory package-user-dir) dir)))
      (unless (or (member dir '("." ".." "archives" "gnupg"))
                  (not (file-directory-p fdir))
                  (not (file-exists-p (concat
                                       (file-name-as-directory fdir) "dir"))))
        (add-to-list 'Info-directory-list fdir)))))

;;;;; = byte-compile - configuration byte-compilation

;; Suppress byte-compilation warnings
(setq byte-compile-warnings nil)
(setq byte-compile-verbose nil)

;;;;; = use-package - package configuration
;; Built in since Emacs version 29 and provides 'easy' package configuration
;; To get statistics on package loading start emacs with emacs --debug-init
;; This will compute the statistics which can be called with
;; use-package-report
(when init-file-debug
  (require 'use-package)
  (setq use-package-compute-statistics t))

;; Not compiling - don't try to install packages
(setq use-package-always-ensure nil)

;;; pure-common.el ends here
