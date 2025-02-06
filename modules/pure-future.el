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

;;;;; = marginalia - display minibuffer meta data
;; Add annotations to the completion buffer
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-t" . marginalia-cycle))
  :hook
  (minibuffer-setup . marginalia-mode))

;;;;; = vundo - graphical undo tree
(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :bind
  ("C-x u" . vundo))

;;;; File Management
;;;; Buffer Management

;;;; Window Management

;;;; Minibuffer and Completion
;;;;; = orderless - complete in any order
(use-package orderless
  :demand
  :custom
  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; Enable partial-completion for files.
  ;; Either give orderless precedence or partial-completion.
  ;; Note that completion-category-overrides is not really an override,
  ;; but rather prepended to the default completion-styles.
  ;; completion-category-overris '((file (styles orderless partial-completion)))
  ;; orderless is tried first
  (completion-category-overrides '(;; partial-completion is tried first
                                   (file (styles partial-completion)))))

;;;;; = embark = emacs mini-buffer actions rooted in keymaps
;; Like a right mmouse click
(use-package embark
  :commands
  (embark-eldoc-first-target)
  :init
  ;; Show the Embark target at point via Eldoc.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :custom
  ;; Replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  (("C-c ." . embark-act)
   ("C-c ;" . embark-dwim)
   ("C-h B" . embark-bindings)))

;;;; Search and Replace

;;;; Keys

;;;; Editing
;;;; Navigation

;;;; Coding

;;;; Programming Languages
;;;;; = aggresive-indent - handle indentations
(use-package aggressive-indent
  :hook
  ((lisp-mode scheme-mode emacs-lisp-mode) . aggressive-indent-mode))

;;;;; = diff-hl - show versioning
(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode))

;;;;; = diff-hl-margin - show versioning in tty
(use-package diff-hl-margin
  :ensure nil
  :unless (display-graphic-p)
  :hook
  (prog-mode . diff-hl-margin-mode))


;;;; Note Taking
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

;;;;; = denote-rename-buffer - rename buffer for modeline purposes.
(use-package denote-rename-buffer
  :ensure nil
  :after (denote)
  :demand t
  :custom
  (denote-rename-buffer-format "%t - %k")
  :config
  (denote-rename-buffer-mode t))

;;;;; = denote-sort - sort denote dired buffers
(use-package denote-sort
  :ensure nil
  :after (denote)
  :demand t
  :commands
  (denote-sort-dired))

;;;;; = denote-journal-extras - journal note taking.
(use-package denote-journal-extras
  :ensure nil
  :after (denote)
  :demand t
  :commands
  (denote-journal-extras-new-entry)
  :custom
  ;; Subfolder where journal notes are saved.
  (denote-journal-extras-direxctory
   (expand-file-name "journal" pure-dir-notes))
  ;; Keyword used for journal notes.
  (denote-journal-extras-keyword "journal")
  ;; Title for journal notes. Nil to prompt for title.
  (denote-journal-extras-title-format 'day-date-month-year-24h)
  :config
  ;; 'Denotify' journal 'dired' too.
  (add-to-list 'denote-dired-directories denote-journal-extras-directory))

;;;;; = denote-org-extras - dynamic org blocks
(use-package denote-org-extras
  :ensure nil
  ;; These commands are handy once a org buffer is opened. Could be a note.
  :after (org)
  :demand t
  :commands
  (denote-org-extras-dblock-insert-links
   denote-org-extras-dblock-insert-missing-links
   denote-org-extras-dblock-insert-backlinks
   denote-org-extras-extract-org-subtree
   denote-org-extras-dblock-insert-missing-links))

;;;;; = denote-silos-extra - multiple isololated note folders
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
(use-package denote-silo-extras
  :ensure nil
  :demand t
  :after (denote)
  :defines
  (pure-dir-private)
  :config
  (add-to-list 'denote-silo-extras-directories
               (expand-file-name "notes_private" pure-dir-private )))

;;;; Shells

;;;; Tools
;;;;; = magit - git porcelain inside emacs
;; Git interface
(use-package magit
  :commands
  (magit-status))

;;;; Security and Privacy

;;;; Communication
;;;; Org Mode
;;;; Pure Functions

;;; Provide
(provide 'pure-future)
;;; pure-future.el ends here
