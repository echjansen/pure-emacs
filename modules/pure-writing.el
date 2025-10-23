;;; pure-writing.el --- Software writing  -*- lexical-binding: t -*-

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

;; This module is dedicated to writing
;; The following tools are implemented:
;; -
;; -
;; -
;; -
                                        ;
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

;;;; Writing tools
;;;;; = olivetti
;; Some keybindings (that have a repeat map)
;; C-c {  - olivetti-shrink
;; C-c }  - olivetti-expand
;; C-c |  - olivetti-set-width
(use-package olivetti
  :commands
  (olivetti-mode)
  :custom
  (olivetti-body-width 120)
  (olivetti-style 'fancy)
  (olivetti-fringe))

;;;;; = writegood-mode
;; Avoid writing text with weasel words, like "many" "various" "very", etc
;; Avoid passive wording
;; Avoid duplicate wording
;; Includes two function to validate the 'effcitiveness' of the text
;; writegood-grade-level and writegood-reading-ease
(use-package writegood-mode
  :init
  :custom-face
  ;;(writegood-weasels-face ((t (:background "red" :foreground "white"))) )
  ;;(writegood-weasels-face ((t (:inherit (word-clock-label)))))
  (writegood-weasels-face ((t :foreground ,(face-foreground 'warning)
                              :background ,(face-background 'warning)
                              :underline t)))
  (writegood-passive-voice-face ((t :foreground ,(face-foreground 'error)
                                    :background ,(face-background 'error)
                                    :underline t)))
  (writegood-duplicates-face ((t :foreground ,(face-foreground 'warning)
                                 :background ,(face-background 'warning)
                                 :strike-through t)))
  :hook
  (text-mode . writegood-mode))

;;;; Note taking tools
;;;;; = denote - lightweight note taking
;; Note taking feature with the following philosophy:
;; - minimalistic using already existing Emacs features
;; - meta data is the file name startegy (date/time, signature,title, keywords)
(use-package denote
  :commands
  (denote
   denote-open-or-create
   denote-open-or-create-with-command)
  :custom
  ;; Main location for notes
  (denote-directory pure-dir-notes)
  ;; Don't save notes on creation. Manually save.
  (denote-save-buffer-after-creation nil)
  ;; Default keywords. Historical notes will add more.
  (denote-known-keywords '("emacs" "linux" "meeting" "project"))
  ;; Use historical keywords
  (denote-infer-keywords t)
  ;; Sort keywords in minibuffer
  (denote-sort-keywords t)
  ;; Org notes by default (text, markdown)
  (denote-file-type nil)
  ;; What fields to provide when creating new notes
  ;; title, keywords, signature, subdirectory, template, date
  (denote-prompts '(title subdirectory keywords template))
  ;; Use historical note fields for new notes selection in minibuffer.
  (denote-history-completion-in-prompts t)
  ;; What fields are propmpted historical to provide for new note creation.
  (denote-prompts-with-history-as-completion
   '(denote-title-prompt
     denote-signature-prompt
     denote-files-matching-regexp-prompt))
  ;; Directories to exclude from all operations
  (denote-excluded-directories-regexp nil)
  ;; Keywords to exclude from all operations
  (denote-excluded-keywords-regexp nil)
  ;; Pick dates with Org's interface
  (denote-date-prompt-use-org-read-date t)
  ;; Confirm note renaming
  (denote-rename-no-confirm t)
  ;; Show the line the backlink is found in.
  (denote-backlinks-show-context t)
  ;; Which 'dired' buffers should be 'denotified'.
  (denote-dired-directories
   (list denote-directory
         (expand-file-name "~/Documents")))
  :hook
  ;; Use colors in 'dired' buffers to identify fields.
  (dired-mode . denote-dired-mode-in-directories))

;;;;; = denote-journal - journal note taking.
(use-package denote-journal
  :after (denote)
  :demand t
  :commands
  (denote-journal-new-entry)
  :custom
  ;; Subfolder where journal notes are saved.
  (denote-journal-direxctory
   (expand-file-name "journal" pure-dir-notes))
  ;; Keyword used for journal notes.
  (denote-journal-keyword "journal")
  ;; Title for journal notes. Nil to prompt for title.
  (denote-journal-title-format 'day-date-month-year-24h)
  :config
  ;; 'Denotify' journal 'dired' too.
  (add-to-list 'denote-dired-directories denote-journal-directory))

;;;;; = denote-org - dynamic org blocks
(use-package denote-org
  :after (org)
  :demand t
  :commands
  ;; These commands are handy once a org buffer is opened. Could be a note.
  (denote-org-link-to-heading
   denote-org-backlinks-for-heading
   denote-org-extract-org-subtree
   denote-org-convert-links-to-file-type
   denote-org-convert-links-to-denote-type
   denote-org-dblock-insert-files
   denote-org-dblock-insert-links
   denote-org-dblock-insert-backlinks
   denote-org-dblock-insert-missing-links
   denote-org-dblock-insert-files-as-headings))

;;;;; = denote-silo - multiple isololated note folders
;; depreciated
;; Have multiple directories for notes that are isolated from each other
;; Each silo requires a hidden file, which contains instructions for denote.
;; ;;; Directory Local Variables.  For more information evaluate:
;; ;;;
;; ;;;     (info "(emacs) Directory Variables")

;; ((nil . ((denote-directory . "/path/to/silo/work")
;;          (denote-known-keywords . ("project" "meeting"))
;;          (denote-infer-keywords . nil)))
;;  (org-mode . ((org-hide-emphasis-markers . t)
;;               (org-hide-macro-markers . t)
;;               (org-hide-leading-stars . t))))
(use-package denote-silo
  :after (denote)
  :disabled
  :defines
  (pure-dir-private)
  :config
  (add-to-list 'denote-silo-directories
               (expand-file-name "notes_private" pure-dir-private )))

;;; Provide:
(provide 'pure-writing)

;;; pure-writing.el ends here
