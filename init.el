;;; init.el --- Pure Emacs initialization. -*- lexical-binding: t -*-

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

;; Early decisions
;; - Fast startup time - achieved with byte compiling and use-package.
;; - Emacs build in features - unless inferior or non existing.
;; - Package.el package manager - has all features required (including vc)

;;; Code:

;; Add the `modules' directory to the load path
(add-to-list 'load-path
             (concat (file-name-as-directory user-emacs-directory) "modules"))

;;;; Compiler settings
(eval-when-compile
  (require 'use-package)
  (if init-file-debug
      (progn
        (message "--debug-init active")
        (setq use-package-compute-statistics t)
        (setq use-package-verbose t)
        (setq use-package-minimum-reported-time 0.001))
    (setq use-package-compute-statistics nil))
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t))

;;;; Package Manager
;;;;; = package.el - package installation
;; Only load  `package.el' and `use-package' when compiling.
;; In interactive mode neither `package.el' nor `use-package' is loaded
;; and the macro expansion from use-package is used (assuming all files are
;; byte compiled).
;; A byte-compiled version of `load-path' is used to find the external packages.

;; Traverse the installed packages and add their paths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        (package-initialize)

        ;; Sources for packages (recipes in this case)
        (setq package-archives
              '(("melpa"        . "https://melpa.org/packages/")
                ("melpa-stable" . "https://stable.melpa.org/packages/")
                ("gnu"          . "https://elpa.gnu.org/packages/")
                ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

        ;; Order of archive priority. The higher the number the higher the priority
        (setq package-archive-priorities
              '(("melpa"         . 4)
                ("gnu"           . 3)
                ("nongnu"        . 2)
                ("melpa-stable"  . 1)))

        ;; use-package configuration during compilation
        (require 'use-package)                    ; When compiling:
        (setq use-package-always-ensure t)        ; Install packages
        (setq use-package-always-defer t)         ; Lazy load when possible
        (setq use-package-compute-statistics nil) ; No statistics in byte-code
        (setq use-package-expand-minimally t)     ; Minimised byte-code

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
;; use-package-report.
;; Note: only reports on files not byte compiled.
(when init-file-debug
  (require 'use-package)
  (setq use-package-compute-statistics t))


(provide 'pure-init)

;;;; Pure Emacs Modules
;;;;; Load the pure emacs common configuration.
(require 'pure-common nil t)

;;;;; Load the pure emacs built-in features
(require 'pure-emacs nil t)

;;;;; Load your external packages
(require 'pure-future nil t)

(provide 'init)
;;; init.el ends here
