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

;;;; Variables
(defvar pure-enable-debug nil
  "non-nil to enable debug.")

;;;; Defer Garbage Collection
(setq gc-cons-threshold most-positive-fixnum)

;;;; File hander
(defvar pure-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq auto-save-list-file-prefix nil)

;;;; Natively compile *.el files (emacs 29>)
(customize-set-variable 'native-comp-async-report-warnings-errors 'silent)
(customize-set-variable 'native-comp-warning-on-missing-source pure-enable-debug)
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-jit-compilation t)

;;;; Performance
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 512 1024))  ; 512kb

;; reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; disable warnings from the legacy advice api. they aren't useful.
(setq ad-redefinition-action 'accept)
(setq warning-suppress-types '((lexical-binding)))

;; by default, emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)

;; a second, case-insensitive pass over `auto-mode-alist' is time wasted.
;; no second pass of case-insensitive search over auto-mode-alist.
(setq auto-mode-case-fold nil)

;; disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; give up some bidirectional functionality for slightly faster re-display.
(setq bidi-inhibit-bpa t)

(unless (or (daemonp) noninteractive)
  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))

  ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
  ;; may introduce down the road.
  (define-advice startup--load-user-init-file (:before (&rest _) init-doom)
    (advice-remove #'load-file #'load-file@silence)))

;;;; When debugging
(setq debug-on-error pure-enable-debug)
(setq jka-compr-verbose pure-enable-debug)

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
(setq-default inhibit-startup-buffer-menu t)
(setq-default inhibit-splash-screen t)
(setq-default initial-scratch-message nil)
(setq-default initial-buffer-choice nil)

;; disable guis because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; remove "for information about gnu emacs..." message at startup
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; suppress the vanilla startup screen completely. we've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)

;;;; Initial Mode
(setq initial-major-mode 'fundamental-mode)

;;;; Load the theme

;; Modus themes are very nice and complete, but have large load times ~0.5 seconds
;;(load-theme 'modus-vivendi)
;;(load-theme 'modus-operandi)
;; Light theme
;;(load-theme 'leuven)
;; dark theme
(load-theme 'wombat)

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
