;;; pure-me.el --- Pure Me Features. -*- lexical-binding: t -*-

;; Copyright (C) 2025 echjansen

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

;; This file contains features used by me, which might be, or not be, of
;; interest to others.
;; I don't see them as 'future' features.

;;; Code:

;;;; Package Configuration
;;;;; = use-package - package configuration macro
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
;;;; Apperance
;;;; Help and Information
;;;; File Management
;;;; Buffer Management
;;;; Window Management
;;;; Minibuffer and Completion
;;;; Search and Replace
;;;; Keys
;;;; Editing
;;;; Navigation
;;;; Coding
;;;; Coding and Programming Languages
;;;; Writing and Note Taking
;;;;; = markdown - markdown-mode
;; Edit markdown files (*.md)
;; Edit github markdown with gfm-mode
;; "C-c" for the mode options (as always)
(use-package markdown-mode
  :commands
  (markdown-mode)
  :mode ("README\\.md\\'" . gfm-mode)
  :mode (".md\\'" . markdown-mode ))

;;;; shells
;;;; tools
;;;; Security and Privacy
;;;; Communication
;;;;; = elfeed-org - Store elfeed sources hierarchically in an org file
(use-package elfeed-org
  :commands
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files (list (concat (file-name-as-directory pure-dir-private) "pure-elfeed.org.gpg"))))

;;;;; = elfeed - RSS reader
(use-package elfeed
  :custom
  (elfeed-search-filter "@1-year-ago +unread ")
  (elfeed-db-directory (expand-file-name "elfeed" pure-dir-cache))
  :config
  ;; Feeds are defined in an org file
  (elfeed-org)
  (elfeed-update))

;;;; Org Mode
;;;; Artificial Intelligence
;;;;; = gptel - a simple LLM client for Emacs
;; Default is ChatGpt (OpenAI)
;; Alternative is local installed Ollama.
;; Some key bindings to know:
;; C-c RET     -> send selection or before point to MML
;; C-u C-c RET -> open transient menu for options
;; C-x C-s     -> save transient options
(use-package gptel
  :commands
  (gptel                                ; Create a chat
   gptel-mode)                          ; Restore a saved chat
  :config
  ;; The code is provided with sub-features, load them
  (require 'gptel-curl nil t)
  (require 'gptel-transient nil t)
  (require 'gptel-rewrite nil t)
  (require 'gptel-ollama nil t)
  ;;Alternative local backend
  (gptel-make-ollama "Ollama"
                     :host "localhost:11434"
                     :stream t
                     :models '(zephyr:latest
                               deepseek-r1:latest))
  :custom
  (gptel-default-mode #'org-mode)
  :bind
  ("C-c RET" . gptel-send))

;;;; Pure Functions

(provide 'pure-me)
;;; pure-me.el ends here
