;;; early-init.el --- Early initialization. -*- no-byte-compile: t -*-

;; Copyright (C) 2024 echjansen

;; This file is part of pure-emacs
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
;; along with pur-emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;; There are benefits in defining GUI related customisation, so as
;; they don't have to be 're-configured' upon startup.

;;; Code:

;;;; Defer Garbage Collection
(setq gc-cons-threshold most-positive-fixnum)

;;;; File hander
(defvar pure-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq auto-save-list-file-prefix nil)

;;;; Native Compilation al found *.el files (emacs 29>)
(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-jit-compilation t)

;;;; Package Management
;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'.
;; Although this configuration doesn't use packages we prevent Emacs from
;; enabling package.el 
(setq package-enable-at-startup nil)

;;;; Set the first (only) frame maximized
(push '(fullscreen . maximized) initial-frame-alist)

;;;; Configure GUI for all frames
(push '(width . 250) default-frame-alist)
(push '(height . 120) default-frame-alist)
(push '(left-fringe . 5) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)
(push '(internal-border-width . 10) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(font . "Source Code Pro:style=medium:size=15") default-frame-alist)

(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)

(setq-default frame-inhibit-implied-resize t)
(setq-default frame-title-format "\n")
(setq-default inhibit-startup-screen t)
(setq-default inhibit-startup-message t)
(setq-default inhibit-splash-screen t)
(setq-default initial-scratch-message nil)

;;;; Initial Mode
(setq initial-major-mode 'fundamental-mode)

;;;; Load the theme
;; Modus themes are very nice and complete, but have large load times ~0.5 seconds
(load-theme 'leuven)
;;(load-theme 'modus-operandi)

;; Reset variables, and message the startup time
(add-hook 'emacs-startup-hook
	  (lambda ()
	    ;; Reset the file handler list
	    (setq file-name-handler-alist pure-file-name-handler-alist)
	    ;; Reset garbage collection
	    (setq gc-cons-threshold 2000000)
	    ;; Startup time message
	    (message (format "Pure-Emacs ready in %.5f seconds with %d garbage collections."
			     (float-time (time-subtract after-init-time before-init-time))
			     gcs-done))))

(provide 'early-init)
;;; early-init.el ends here

