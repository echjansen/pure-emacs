;;; pure-future.el --- Pure Emacs's Future. -*- lexical-binding: t -*-

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

;; This file is an addition to pure-emacs.
;; This file contains packages that I would expect to be part of Emacs
;; in the future.
;; The same principles apply to this configuration:
;; - Fast start-up time.
;; - Configuration runs out of the box.
;; - No key-bindings modified.

;;; Code:

;;;; Package Management

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
	      '(("elpa"       . "https://elpa.gnu.org/packages/")
		("elpa-devel" . "https://elpa.gnu.org/devel/")
		("nongnu"     . "https://elpa.nongnu.org/nongnu/")
		("melpa"      . "https://melpa.org/packages/")))

	;; Order of archive priority. The higher the number the higher the priority
	(setq package-archive-priorities
	      '(("elpa-del" . 3)
		("melpa"    . 2)
		("nongnu"   . 1)))

	;; use-package configuration during compilation
	(require 'use-package)
	(setq use-package-always-ensure t)
	(setq use-package-always-defer t)
	(setq use-package-compute-statistics nil)
	(setq use-package-expand-minimally t)
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


;;;; Emacs

;;;; Apperance

;;;; Help and Information

;;;;; = helpful - more information to help
;; Improved help information.
(use-package helpful
  :init
  ;; Display helpfull to the right in side-window
  (add-to-list
   'display-buffer-alist
   `("^\\*helpful.*\\*$"
     (display-buffer-in-side-window)
     (dedicated . t)
     (side . right)
     (window-width . 0.5)
     (slot. 0)
     (body-function . pure-window-select)))
  :bind
  (("C-h f"   . helpful-callable)
   ("C-h v"   . helpful-variable)
   ("C-h k"   . helpful-key)
   ("C-h x"   . helpful-command)
   ("C-h ."   . helpful-at-point)
   ("C-h F"   . helpful-function)))

;;;;; = marginalia
;; Add annotations to the completion buffer
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-t" . marginalia-cycle))
  :hook
  (minibuffer-setup . marginalia-mode))

;;;;; = which-key
;; show available keys for current mode
(use-package which-key
  :custom
  ;; Show which-key on bottom of frame
  (which-key-side-window-location 'bottom)
  ;; Sort alphabetically
  (which-key-sort-order 'which-key-key-order-alpha)
  ;; Max height
  (which-key-side-window-max-width 0.33)
  ;; Relax, I know what I am doing most of the time
  (which-key-idle-delay 1.0)
  :hook
  (after-init . which-key-mode))

;;;; File Management

;;;; Buffer Management

;;;; Window Management

;;;; Minibuffer

;;;; Search and Replace

;;;; Keys

;;;; Editing

;;;; Navigation

;;;; Coding

;;;; Programming Languages

;;;; Shells

;;;; Tools

;;;; Security and Privacy

;;;; Communication

;;;; Org Mode

;;;; Pure Functions

;;; Provide
(provide 'pure-future)
;;; pure-future.el ends here
