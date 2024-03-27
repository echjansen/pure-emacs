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

;;;;; = simple - modeline info and line display
(use-package simple
  :ensure nil
  :hook
  (;; Show points line number
   (after-init . line-number-mode)
   ;; Show points column number
   (after-init . column-number-mode)
   ;; Long lines will wrap on buffer edge
   (text-mode . visual-line-mode)))

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

;;;; Help and Information

;;;;; = help - always select the help windows
(use-package help
  :ensure nil
  :custom
  ;; Always select the help windows - when called
  (help-window-select t))

;;;;; = eldoc - echo area context at point information
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

;;;;; = transient - menus and options
;; The menu package known from Magit has been included in Emacs 29.1.
;; Although not a replacement for 'which-key,
;; it opens up menu functionality for existing and future packages.
(use-package transient
  :ensure nil
  :commands (transient-prefix)
  :custom
  (transient-history-file (concat pure-dir-cache "trans-history.el"))
  (transient-levels-file  (concat pure-dir-cache "trans-level.el"))
  (transient-values-file  (concat pure-dir-cache "trans-value.el")))

;;;;; = apropos - find symbols, functions, variables, etc
;; Keybinding "C-h a"
(use-package apropos
  :ensure nil
  :custom
  (apropos-do-all t)
  (apropos-sort-by-scores 'show-scores)
  (apropos-compact-layout t))

;;;; File Management

;;;;; = files - files and backups
(use-package files
  :ensure nil
  :custom
  (backup-directory-alist (list (cons "."  pure-dir-backup)))
  (make-backup-files nil)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 5)
  (version-control t)
  (auto-save-default nil))

;;;;; = recentf - recently opened files
;; Maintains a list of recently opened files
(use-package recentf
  :ensure nil
  :custom
  ;; Where to save the recentf file - in the .cache
  (recentf-save-file (expand-file-name "recentf" pure-dir-cache))
  ;; Remove duplicates on mode change
  (recentf-auto-cleanup 'mode)
  ;; Max number of files saved
  (recentf-max-saved-items 100)
  ;; Max number of files served in files menu
  (recentf-max-menu-items 10)
  :hook
  (after-init . recentf-mode)
  :bind
  ("C-x C-r" . recentf))

;;;;; = saveplace - last position in file
;; Save point position in files between sessions.
(use-package saveplace
  :ensure nil
  :custom
  ;; Where to save the saveplaces file - in the .cache
  (save-place-file (expand-file-name "saveplaces" pure-dir-cache))
  (save-place-forget-unreadable-files t)
  :hook
  (after-init . save-place-mode))

;;;;; = dired - file management
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t))

;;;;; = uniquify - files with the same name get directory included.
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

;;;; Window Management

;;;;; = windmove - reposition buffers
(use-package windmove
  :ensure nil
  :bind (("<S-left>"  . windmove-left)
         ("<S-right>" . windmove-right)
         ("<S-up>"    . windmove-up)
         ("<S-down>"  . windmove-down)
	 ("<C-left>"  . windmove-swap-states-left)
	 ("<C-right>" . windmove-swap-states-right)
         ("<C-up>"    . windmove-swap-states-up)
         ("<C-down>"  . windmove-swap-states-down)))

;;;;; = winner - undo / redo window order
;; C-c <left> = undo, C-c <right> redo undo
(use-package winner
  :ensure nil
  :hook
  (after-init . winner-mode))

;;;; Minibuffer

;;;;; = savehist - last minibuffer commands used
;; Persist emacs minibuffer history
(use-package savehist
  :ensure nil
  :custom
  ;; Where to save the savehsit file - in the .cache
  (savehist-file (expand-file-name "savehist" pure-dir-cache))
  :hook
  (after-init . savehist-mode))

;;;;; = icomplete - vertical completion buffer
;; Minibuffer completion UI build-in Emacs
(use-package icomplete
  :ensure nil
  :custom
  ;; Automatically delete superfluous parts of file names
  (icomplete-tidy-shadowed-file-names t)
  ;; Show completions when first prompting for input
  (icomplete-show-matches-on-no-input t)
  ;; Pending completions over which to apply compute-delay
  (icomplete-delay-completions-threshold 50)
  ;; Maximum height completion buffer
  (completions-max-height 10)
  (read-buffer-completion-ignore-case t)
  ;; Generally ignore case when completing
  (completion-ignore-case t)
  ;; Ignore case for buffer names during completion
  (read-buffer-completion-ignore-case t)
  ;; Ignore case for file names during completion
  (read-file-name-completion-ignore-case t)
  ;; vertical completion minibuffer with fuzzy finding
  (completion-styles '(partial-completion substring initials flex))
  ;; typre specific over-rides
  (completion-category-overrides '((file (styles . (partial-completion substring)))
                                   (buffer (styles . ( basic substring partial-completion)))
                                   (project-file (styles . (partial-completion substring)))
                                   (info-menu (styles . (substring)))))
  :config
  (add-to-list 'completion-ignored-extensions ".eln")
  :hook
  (after-init . fido-vertical-mode)
  ;; Usefull mapping
  :bind (:map icomplete-fido-mode-map
              ("RET" . icomplete-fido-ret)
              ("TAB" . icomplete-force-complete)))

;;;; Search and Replace
;;;;; = isearch - find
;; Some interesting search options:
;; C-s C-h C-h : isearch key-bindingas (many)
;; M-s M-.     : search for object at point
;; M-n or M-p  : move up or down in isearch history
(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-highlight t)
  (isearch-repeat-on-direction-change t)
  ;; Use M-p and M-n to show history
  (isearch-wrap-pause t)
  (search-ring-max 10)
  ;; Activate with M-s M-.
  (isearch-forward-thing-at-point '(region url symbol sexp email))
  (isearch-allow-prefix t))

;;;;; = replace - contains occur
;; Some interesting key-bindings:
;; M-s o   : occur
(use-package replace
  :ensure nil
  :init
  ;; Place the occur buffer on the bottom
  (add-to-list
   'display-buffer-alist
   `("\\*Occur\\*"
     ;; Display functions (list in order)
     (display-buffer-reuse-mode-window
      display-buffer-below-selected)
     ;; Parameters
     (window-height . 10)
     (dedicated .t)
     (body-function . pure-window-select))))

;;;; Keys

;;;;; - repeat - why repeat complex key combinations
(use-package repeat
  :ensure nil
  :hook
  (after-init . repeat-mode))

;;;; Editing

;;;;; = ispell - spell checking (install Aspell)
;; Spelling checker program 'aspell' must be installed externally
(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "en_US"))

;;;;; = flyspell - on the fly spell checking
;; Performs spelling check while editing
;; Any wrong words are underlined
(use-package flyspell
  :ensure nil
  :custom
  ;; Add correction to abbrev table
  (flyspell-abbrev-p t)
  ;; Don't emmit messages when checking words
  (flyspell-issue-message-flag nil)
  ;; Don't display a welcome message when started
  (flyspell-issue-welcome-flag nil)
  :bind (:map flyspell-mode-map
	      ("C-;" . nil)
              ("C-," . flyspell-goto-next-error)
              ("C-." . flyspell-auto-correct-word))
  :hook
  ;; Spelling check for text modes
  (text-mode . flyspell-mode)
  ;; Spelling check for comments and strings
  (prog-mode . flyspell-prog-mode))

;;;;; = dictionary - look up words for meaning (on-line)
;; Quickly search for reference with M-.
;; Install with:
;; yay -S dictd dict dict-{wn,vera,jargon,devil,gcide,foldoc}
;; sudo systemctl enable dictd
(use-package dictionary
  :ensure nil
  :custom
  ;; Remote dictionary
  ;; (dictionary-server "dict.org")
  ;; Local dictionary
  (disctionary-servevr "localhost")
  :bind
  (:map text-mode-map
	("M-." . dictionary-lookup-definition)))

;;;;; = abbrev - replace acronims with full word
;; acronims could also be spelling mistakes
;; TODO - not starting - 'abbrev-mode t'
(use-package abbrev
  :ensure nil
  :custom
  (abbrev-file-name (expand-file-name "abbrev_defs" pure-dir-private))
  (save-abbrevs 'silently)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  :hook
  ;; Activate abbrev in text-mode and prog-mode
  ((text-mode prog-mode) . abbrev-mode))

(provide 'pure-emacs)
;;; pure-emacs.el ends here
