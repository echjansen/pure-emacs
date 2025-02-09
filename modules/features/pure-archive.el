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


;;;; Emacs

;;;; Apperance
;;;;; = pure-line - modeline or headerline
(use-package pure-line
  :disabled
  :ensure nil
  :commands
  (pure-line-mode)
  :custom
  (display-time-day-and-date nil)
  (pure-line-position 'top)      ;; Set position of status-line
  (pure-line-abbrev t)           ;; abbreviate major modes
  (pure-line-hspace "  ")        ;; add some cushion
  (pure-line-prefix t)           ;; use a prefix symbol
  (pure-line-prefix-padding nil) ;; no extra space for prefix
  (pure-line-status-invert t)    ;; no invert colors
  (pure-line-space-top +.2)      ;; padding on top and bottom of line
  (pure-line-space-bottom -.2)
  (pure-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  :hook
  (emacs-startup . pure-line-mode))

;;;;; = telephone-line - mode line alternative
(use-package telephone-line
  :disabled
  :custom
  ;; Segments
  (telephone-line-lhs
   '((nil    . (telephone-line-buffer-segment))
     (accent . (telephone-line-vc-segment
                telephone-line-erc-modified-channels-segment
                telephone-line-process-segment))
     (nil    . (telephone-line-minor-mode-segment))))

  (telephone-line-rhs
   '((nil    . (telephone-line-airline-position-segment))
     (accent . (telephone-line-major-mode-segment))
     (evil   . (telephone-line-misc-info-segment))))
  ;; Presentation
  (telephone-line-primary-left-separator 'telephone-line-cubed-left)
  (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
  (telephone-line-primary-right-separator 'telephone-line-cubed-right)
  (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-height 24)
  ;; (telephone-line-evil-use-short-tag t)
  :hook
  (emacs-startup . telephone-line-mode))

;;;;; = doom-Mode line
(use-package doom-modeline
  :vc t
  :hook
  (after-init . doom-modeline-mode))

;;;; File Management
;;;;; = dired-subtree - browse folders in a single view
(use-package dired-subtree
  :after dired
  :commands
  (dired-subtree-toggle
   dired-subtree-remove)
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
        ("<tab>"     . dired-subtree-toggle)
        ("TAB"       . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)
        ("S-TAB"     . dired-subtree-remove)))

;;;; Buffer Management

;;;; Window Management

;;;; Minibuffer and Completion
;;;;; = vertico - VERTical Interactive COmpletion
;; Current version on Melpa has an issue with compiling.
;; Loading vertico from source. Including the extensions.
(use-package vertico
  ;; :vc (:url "https://github.com/minad/vertico")
  ;; :load-path "elpa/vertico/extensions"
  :custom
  ;; Different scroll margin
  (vertico-scroll-margin 0)
  ;; Show more candidates
  (vertico-count 10)
  ;; Grow and shrink the minibuffer
  (vertico-resize t)
  :config
  ;; Turn of pure-emacs enabled completion
  (fido-mode -1)
  (fido-vertical-mode -1)
  :hook
  (after-init . vertico-mode))

;;;;; = vertico-directory - directory navigation
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;;;; = vertico-buffer - display Vertico as a regular buffer in a large window.
(use-package vertico-buffer
  :ensure nil
  :demand
  :commands
  (vertico-buffer-mode))

;;;;; = vertico-grid - grid display for Vertico.
;; display the minibuffer in grid mode
(use-package vertico-grid
  :ensure nil
  :demand
  :commands
  (vertico-grid-mode))

;;;;; = vertico-reverse - reverse the Vertico display.
(use-package vertico-reverse
  :ensure nil
  :demand
  :commands
  (vertico-reverse-mode))

;;;;; = vertico-unobtrusive - unobtrusive display for Vertico.
(use-package vertico-unobtrusive
  :ensure nil
  :demand
  :commands
  (vertico-unobtrusive-mode))

;;;;; = vertico-flat - flat, horizontal display for Vertico.
(use-package vertico-flat
  :ensure nil
  :demand
  :commands
  (vertico-flat-mode))

;;;;; = vertico-quick - quick keys
;; Avy style quick selection when within vertico (mini)buffer.
(use-package vertico-quick
  :ensure nil
  :demand
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert)
              ("C-q" . vertico-quick-exit)))

;;;;; = vertico-multiform - configure Vertico in various forms per command.
;; Additional keys in minibuffer
;; M-B -> vertico-multiform-buffer
;; M-F -> vertico-multiform-flat
;; M-G -> vertico-multiform-grid
;; M-R -> vertico-multiform-reverse
;; M-U -> vertico-multiform-unobtrusive
;; M-V -> vertico-multiform-vertical
(use-package vertico-multiform
  :ensure nil
  :init
  (defun pure-sort-directories-first (files)
    "Sort the directory FILES with directories first."
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  :commands
  (vertico-grid-mode)
  :custom
  ;; Determine the minibuffer style per command
  (vertico-multiform-commands
   '((execute-extended-command buffer)))
  ;; Determine the minibuffer style per mode
  (vertico-multiform-categories
   '((file  reverse (vertico-sort-function . pure-sort-directories-first))
     (imenu buffer)
     ;; Provides a which-key like buffer "C-x C-h, or any other prefix."
     (embark-keybinding grid)
     (t     reverse)))
  :hook
  (after-init . vertico-multiform-mode))

;;;;; = corfu - Completion in Region FUnction
;; popop completion for programming, spelling, dabbrev, etc (see capf)
;; Emacs and the extention 'Cape' provide completion backends (capf)'
(use-package corfu
  :custom
  ;; Enable auto completion
  (corfu-auto t)
  ;; Automatically quit when no match found
  (corfu-quit-no-match t)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (eglot-managed-mode . corfu-mode)))

;;;;; = corfu-echo - display brief info of candidate in echo.
(use-package corfu-echo
  :ensure nil
  :hook
  (corfu-mode . corfu-echo-mode))

;;;;; = corfu-info - display candidate help or source-code.
;; M-h - while in corfu, provides help on selected candidate.
;; M-g - while in corfu, provides source code of the selected candidate.
(use-package corfu-info
  :ensure nil
  :after corfu)

;;;;; = corfu-history - display used candidates first
(use-package corfu-history
  :ensure nil
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history)
  :hook
  (corfu-mode . corfu-history-mode))

;;;;; = corfu-terminal - tty support for corfu
(use-package corfu-terminal
  :unless (display-graphic-p)
  :hook
  (corfu-mode-hook . corfu-terminal-mode))

;;;;; = cape - completion at point extensions
;; Defines what 'information' to include when trying to complete-at-point
(use-package cape
  :disabled
  :bind ("C-c p" . cape-prefix-mOBap)
  ;; The order of the functions matters, the first function returning a result
  ;; wins.  Note that the list of buffer-local completion functions
  ;; takes precedence over the global list.
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  :init
  (defun pure--cape-setup-elisp ()
    (setf (elt (cl-member 'elisp-completion-at-point
                          completion-at-point-functions) 0)
          #'elisp-completion-at-point)
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
    (add-to-list 'completion-at-point-functions #'cape-file))

  (defun pure--cape-setup-org ()
    (add-to-list 'completion-at-point-functions #'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (add-to-list 'completion-at-point-functions #'ispell-completion-at-point))

  (defun pure--cape-setup-text ()
    (add-to-list 'completion-at-point-functions #'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

  :hook
  ;; Register the functions via hooks so one has control over the
  ;; the lookup functions depending on the mode.
  ((emacs-lisp-mode . pure--cape-setup-elisp)
   (org-mode        . pure--cape-setup-org)
   (text-mode       . pure--cape-setup-text)))

;;;; Search and Replace

;;;; Keys

;;;; Editing
;;;;; = jinx - modern spelling checker
;; wrapper around many different spelling checkers such as ispell, aspell,
;; and hunspell.
;; Requires the install of an external library 'enchant'
;; TODO - how to configure libraries, etc
(use-package jinx
  :disabled
  :custom
  (jinx-languages "en_AU")
  :config
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 25)))
  (vertico-multiform-mode 1)

  ;; Add misspelled words to 'abbrev
  (defun jinx--add-to-abbrev (overlay word)
    "Add abbreviation to `global-abbrev-table'.
The misspelled word is taken from OVERLAY.  WORD is the corrected word."
    (let ((abbrev (buffer-substring-no-properties
                   (overlay-start overlay)
                   (overlay-end overlay))))
      (message "Abbrev: %s -> %s" abbrev word)
      (define-abbrev global-abbrev-table abbrev word)))

  (advice-add 'jinx--correct-replace :before #'jinx--add-to-abbrev)
  :hook
  (text-mode . jinx-mode)
  :bind (:map jinx-mode-map
              ("C-;" . nil)
              ("C-," . jinx-next)
              ("C-." . jinx-correct)))

;;;; Navigation

;;;; Coding

;;;; Programming Languages
;;;;; = geiser-guile - a ~lisp~ language based on ~scheme~
(use-package geiser-guile
  :commands
  (geiser-guile)
  :custom
  (scheme-program-name "guile")
  (geiser-repl-history-filename (concat pure-dir-cache "geiser-history")))

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
  :config
  (add-to-list 'denote-silo-extras-directories
               (expand-file-name "notes_private" pure-dir-private )))

;;;; Shells

;;;; Tools
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
;;;;; = ox-pandoc - org mode file type exporter
(use-package ox-pandoc
  :vc (:url "https://github.com/emacsorphanage/ox-pandoc")
  :after org
  :demand t)

;;;;; = toc-org - create a index in an org-file
(use-package toc-org
  :hook
  (org-mode . toc-org-mode))

;;;; Pure Functions

;;; Provide
(provide 'pure-future)
;;; pure-future.el ends here
