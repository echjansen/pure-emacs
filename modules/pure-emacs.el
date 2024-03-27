;;; pure-emacs.el --- Pure Emacs initialization. -*- lexical-binding: t -*-

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
;; - one single configuration file for built-in emacs features
;; - easy to copy, so it can be used by other Emacs configs.
;; - use-package for package management, as it is built-in (Emacs 29.1).
;; - optimise start-up time by byte-compiling.

;;; Code:

;;;; Package Configuration

;;;;; = use-package - package configuration macro
;; Built-in Emacs since version 29 and provides a 'consistent' package
;; configuration.
;; To get statistics on package load times start Emacs with --debug-init.
;; This will compute the statistics which can be called with
;; M-X use-package-report
;; Note the eval-when-compile - extracting use-package only during
;; compilation. The expanded code is used during evaluation.
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

;;;; Emacs
;;;;; = emacs - sensible defaults
(use-package emacs
  :ensure nil
  :preface
  (prefer-coding-system 'utf-8)
  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
  :custom
  ;; Cursor style
  (cursor-type 'box)
  (visible-bell t)
  (custom-file (concat pure-dir-cache "pure-custom.el"))
  :config
  (setq-default indent-tabs-mode nil)
  (setq tab-width 2)
  (setq default-tab-width 2)
  :bind
  ;; Don't close Emacs
  ("C-z" . nil))

;;;; Apperance

;;;;; = timer - display time
;; Display the time in the modeline.
(use-package timer
  :ensure nil
  :custom
  (display-time-day-and-date nil)
  (display-time-24hr-format t)
  (display-time-interval 1)
  (display-time-format "%y-%m-%d %H:%M")
  :hook
  (after-init . display-time))

;;;;; = display-fill-column-indicator
;; Show line in buffer to show fill-column boundary
(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (fill-column 80)
  :hook
  (emacs-lisp-mode . display-fill-column-indicator-mode))

;;;;; = display-line-numbers
;; Display line numbers
(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-type 'relative)
  :hook
  (prog-mode . display-line-numbers-mode))

;;;;; = hl-line - highlight line at point
(use-package hl-line
  :ensure nil
  :config
  ;; Some themes use underline for highlighting. Remove it. 
  (set-face-attribute 'highlight nil :underline nil)
  :hook
  (after-init . global-hl-line-mode))

(provide 'pure-emacs)
;;; pure-emacs.el ends here
