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
;; - one single configuration file for built-in Emacs features
;; - easy to copy, so it can be used by other Emacs configs.
;; - use-package for package management, as it is built-in (Emacs 29.1).
;; - optimise start-up time by byte-compiling.

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
;;;;; = emacs - sensible defaults
(use-package emacs
  :ensure nil
  :no-require
  :preface
  (prefer-coding-system 'utf-8)
  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
  :custom
  ;; Cursor style
  (cursor-type 'box)
  (visible-bell t)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  :config
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  ;; Enable these commands that are by default disabled and produce a warning.
  (mapc
   (lambda (command)
     (put command 'disabled nil))
   '(;; Narrowing functions
     narrow-to-region narrow-to-page narrow-to-defun
     ;;Upper/lower case functions
     upcase-region downcase-region))
  :bind
  ;; Don't close Emacs
  ("C-z" . nil))

;;;;; = cus-edit - Emacs customisation
;; Overriding pure-emacs default settings with 'customize-save-variable'.
(use-package cus-edit
  :ensure nil
  :demand t
  :custom
  (custom-file pure-custom-file)
  :hook
  (emacs-startup . (lambda () (load custom-file))))

;;;;; = comp-run - native compilation at run time
(use-package comp-run
  :ensure nil
  :custom
  ;; Ask when exiting Emacs and compilation in progress (avoid tmp's)
  (native-comp-async-query-on-exit t))

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
   (text-mode . visual-line-mode)
   ;; Long lines will not wrap
   (prog-mode . toggle-truncate-lines)))

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



;;;;; = modeline - modeline configuration
(use-package modeline
  :ensure nil
  :no-require
  :init
  (setq  major-mode-abbrev-list
         '((emacs-lisp-mode  . "")
           (python-mode      . "")
           (python-ts-mode   . "")
           (dired-mode       . "")
           (org-mode         . "")
           (helpful-mode     . "")
           (fundamental-mode . "F")))

  (setq-default
   mode-line-format
   '("%e"
     ;; Buffer is read-only
     (:eval (if buffer-read-only
                (propertize "  " 'face
                            '(:background "yellow" :foreground "black"))))

     ;; Buffer dirty indication
     (:eval (if (and (buffer-file-name) (buffer-modified-p))
                (propertize " " 'face
                            '(:background "red" :foreground "white"))))

     ;; Major mode symbol
     (:eval
      (when (mode-line-window-selected-p)
        (format " %s"
                (propertize
                 (capitalize (major-mode-abbreviation))
                 'face '(:foreground "white")))))

     ;; Buffer narrowed indications
     (:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face '(:background "blue" :foreground "white"))))

     ;; Buffer name
     "  " mode-line-buffer-identification

     ;; Version control status
     (:eval
      (when (and vc-mode (mode-line-window-selected-p))
        (let* ((backend (vc-backend (buffer-file-name)))
               (branch (substring-no-properties
                        vc-mode (+ (length (symbol-name backend)) 2)))
               (state (vc-state (buffer-file-name))))
          (concat
           (propertize
            (format "   %s"
                    branch)
            'face '(:inherit bold))
           (when state
             (if (string= state "edited")
                 (propertize
                  (format ":%s " state) 'face '(:inherit bold))
               (format ":%s " state)))))))

     ;; Show flymake diagnostics when available
     (:eval
      (when (and flymake-mode (mode-line-window-selected-p))
        flymake-mode-line-title
        flymake-mode-line-exception
        flymake-mode-line-counters))

     ;; Remainder on the right
     mode-line-format-right-align

     ;; Position in Buffer
     (:eval
      (when (mode-line-window-selected-p)
        "  [%l:%C:%o]  "))

     ;; Time and miscellaneous
     (:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info)))))

(defun major-mode-abbreviation ()
  "Return the abbreviation of the current major mode based on `mode-list`.
If the major mode is not in `mode-list`, return an empty string."
  (let ((mode (intern (symbol-name major-mode))))
    (or (cdr (assoc mode major-mode-abbrev-list)) "")))

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
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

;;;;; = transient - menus and options
;; The menu package known from Magit has been included in Emacs 29.1.
;; Although not a replacement for 'which-key,
;; it opens up menu functionality for existing and future packages.
(use-package transient
  :if (version< "28.0" emacs-version)
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

;;;;; = help-fns - complicated help functions
(use-package help-fns
  :ensure nil
  :bind
  ("C-h F" . describe-face))

;;;;; = which-key - discover keybindings
;; Popup minibuffer with available keybindings sorted by current mode first.
;; ctl-x-map          - C-x (global key map-sym)
;; mode-specific-map  - C-c (user defined map)
(use-package which-key
  :unless (version<= emacs-version "30.0.50")
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 0.5)
  (which-key-sort-order 'which-key-local-then-key-order)
  :config
  ;; Naming the vanilla Emacs prefixes
  (define-key ctl-x-map "a" (cons "abbrev" abbrev-map))
  (define-key ctl-x-map "n" (cons "narrow" narrow-map))
  (define-key ctl-x-map "p" (cons "project" project-prefix-map))
  (define-key ctl-x-map "r" (cons "register" narrow-map))
  (define-key ctl-x-map "t" (cons "tab" tab-prefix-map))
  (define-key ctl-x-map "v" (cons "versioning" vc-prefix-map))
  (define-key ctl-x-map "w" (cons "window" window-prefix-map))
  :hook
  (after-init . which-key-mode))

;;;; File Management

;;;;; = files - files and backups
(use-package files
  :ensure nil
  :custom
  ;; Where to save to backuo file - in the backup dir
  (backup-directory-alist (list (cons "."  pure-dir-backup)))
  ;; Control if backups are made.
  (make-backup-files nil)
  ;; Always backup by copying
  (backup-by-copying t)
  ;; Delete old backup files
  (delete-old-version t)
  ;; Keep 5 backup files
  (kept-new-versions 5)
  ;; Make numberic backup versions
  (version-control t)
  ;; Do not automatically save
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

;;;; Buffer Management
;;;;; = ibuffer - buffer management
;; C-c b    - buffer selection using the minibuffer.
;; C-x C-b  - buffer selection with info using ibuffer.
(require 'cl-seq)
(use-package ibuffer
  :ensure nil
  :commands
  (ibuffer-switch-to-saved-filter-groups)
  :defines
  (ibuffer-saved-filter-groups)
  :functions
  (pure--ibuffer-get-major-modes-list
   pure--ibuffer-generate-filter-groups-alist
   ibuffer-vc--status-string
   ibuffer-vc--state)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-display-summary nil)
  :config

  ;; Display vc status info in the ibuffer list.
  (defun ibuffer-vc--state (file)
    "Return the `vc-state' for FILE, or `nil' if unregistered."
    (ignore-errors (vc-state file)))

  (defun ibuffer-vc--status-string ()
    "Return a short string to represent the current buffer's status."
    (when buffer-file-name
      (let ((state (ibuffer-vc--state buffer-file-name)))
        (if state
            (symbol-name state)
          "-"))))

  ;; Use human readable Size column instead of original one.
  (define-ibuffer-column pure--size
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000)
      (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000)
      (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t
      (format "%8d" (buffer-size)))))

  (define-ibuffer-column vc-status-mini
    (:name "V")
    (if buffer-file-name
        (let ((state (ibuffer-vc--state buffer-file-name)))
          (cond
           ((eq 'added state) "A")
           ((eq 'removed state) "D")
           ((eq 'up-to-date state) "U")
           ((eq 'edited state) "E")
           ((eq 'needs-update state) "N")
           ((memq state '(conflict needs-merge unlocked-changes)) "C")
           ((eq 'ignored state) "!")
           ((memq state '(() unregistered missing)) "?")))
      " "))

  (define-ibuffer-column vc-status
    (:name "VC status")
    (ibuffer-vc--status-string))

  ;; Modify the default ibuffer-formats.
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (pure--size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))

  (defun pure--ibuffer-get-major-modes-list ()
    "Get all major modes based on opened buffers."
    (mapcar
     (lambda (buffer)
       (buffer-local-value 'major-mode (get-buffer buffer)))
     (buffer-list (selected-frame))))

  (defun pure--ibuffer-generate-filter-groups-alist (mm-list result-list)
    "Create an alist of filtering groups to switch between."
    (if mm-list
        (let* ((cur-mm (car mm-list))
               (next-res-list-el `(,(capitalize
                                     ;; Trim `-mode' string.
                                     (substring (symbol-name cur-mm) 0 -5))
                                   (mode . ,cur-mm))))
          (pure--ibuffer-generate-filter-groups-alist
           (cdr mm-list) (cons next-res-list-el result-list)))
      result-list))

  (defun pure--ibuffer-generate-filter-groups-by-major-mode ()
    "Generate `ibuffer-saved-filter-groups' by major mode."
    (let* ((ignore-modes '(Buffer-menu-mode
                           compilation-mode
                           minibuffer-inactive-mode
                           ibuffer-mode
                           magit-process-mode
                           messages-buffer-mode
                           fundamental-mode
                           completion-list-mode
                           help-mode
                           Info-mode))
           (groups
            (list
             (cons "default"
                   (pure--ibuffer-generate-filter-groups-alist
                    ;; Created by major mode.
                    (cl-set-difference
                     (cl-remove-duplicates
                      (pure--ibuffer-get-major-modes-list))
                     ignore-modes)
                    ;; Manually created.
                    '(("Modified" (predicate buffer-modified-p
                                             (current-buffer)))))))))
      (setq ibuffer-saved-filter-groups groups)
      (ibuffer-switch-to-saved-filter-groups "default")))
  :bind
  ([remap list-buffers] . ibuffer)
  :hook
  ((ibuffer-mode . (lambda ()
                     (ibuffer-switch-to-saved-filter-groups "default")))
   ;; Update filter group when calling `ibuffer'.
   (ibuffer . pure--ibuffer-generate-filter-groups-by-major-mode)))

;;;; Window Management

;;;;; = window - resizing
;; Window resizing is made available via
;; C-x } - widen
;; C-x { - narrow
;; C-x ^ - enlarge
;; C-x v - shrink
;; Note that they are connected to resize-window-repeat-map
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

;;;; Minibuffer and Completion
;;;;; = minibuffer - selection window
(use-package minibuffer
  :ensure nil
  :custom
  ;; Only show M-x commands that work with current major mode
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Completion Styles
  (completion-styles
   '(basic substring partial-completion flex))
  ;; Completion style overrides
  (completion-category-overrides
   '((file (styles . (partial-completion substring)))
     (buffer (styles . ( basic substring partial-completion)))
     (project-file (styles . (partial-completion substring)))
     (info-menu (styles . (substring)))))
  ;; Completions
  (completions-format 'horizontal)
  (completions-sort 'historical)
  (completion-cycle-threshold t)
  (completion-flex-nospace nil)
  (completion-show-help nil)
  (completion-pcm-complete-word-inserts-delimiters t)
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (resize-mini-windows t)
  (completions-max-height 10)
  :config
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

;;;;; = icomplete - vertical completion buffer
;; Minibuffer completion UI build-in Emacs
(use-package icomplete
  :disabled
  :ensure nil
  :demand t
  :custom
  ;; Automatically delete superfluous parts of file names
  (icomplete-tidy-shadowed-file-names t)
  ;; Show completions when first prompting for input
  (icomplete-show-matches-on-no-input t)
  ;; Pending completions over which to apply compute-delay
  (icomplete-delay-completions-threshold 50)
  ;; How initial completion work for types
  (completion-category-overrides
   '((buffer (styles substring flex) (cycle 3))
     (files  (styles flex) (cycle 5))))
  (icomplete-in-buffer t)
  ;; How ALL completions work
  (completion-styles
   '(partial-completion basic))
  :config
  (add-to-list 'completion-ignored-extensions ".eln")
  ;; Don't start on a 'after-init hook' to allow disabling in pure-future
  (fido-vertical-mode)
  ;; Usefull mapping
  :bind (:map icomplete-fido-mode-map
              ("RET" . icomplete-fido-ret)
              ("TAB" . icomplete-force-complete)))

;;;;; = completion-preview - complete at point
;; Display a completion suggestion for the symbol at point
;; Can be used in combination with packages such as 'corfu' it
;; appears to have more use during writing text.
(use-package completion-preview
  :unless (version<= emacs-version "30.0.50")
  :custom
  (completion-preview-minimum-symbol-length 2)
  :bind (:map completion-preview-active-mode-map
              ("<tab>"   . completion-preview-insert)
              ("<right>" . completion-preview-insert)
              ("M-p" . completion-preview-prev-candidate)
              ("M-n" . completion-preview-next-candidate)
              ("M-f" . completion-preview-insert-word))
  :hook
  ((text-mode org-mode prog-mode) . completion-preview-mode))

;;;;; = savehist - last minibuffer commands used
;; Persist emacs minibuffer history
(use-package savehist
  :ensure nil
  :custom
  ;; Where to save the savehsit file - in the .cache
  (savehist-file (expand-file-name "savehist" pure-dir-cache))
  :hook
  (after-init . savehist-mode))

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

;;;;; = ispell - spell checking (install hunspell)
;; Spelling checker program 'aspell' must be installed externally
;; Spell check is controlled by flyspell.
;; ispell-dictionary-alist has all language variants 'pre-assigned'
;; Use 'M-x ispell-change-dictionary' to select language
;; Keybindings to remember:
;; "C-,"   - go to next misspelled word
;; "C-."   - correct (or loop) to misspelled word
;; "C-c $" - show misspelled dropdown
(use-package ispell
  :ensure nil
  :if (executable-find "aspell")
  :custom
  ;; If using hunspell, the dictionay file must already exist!
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_AU"))
  (ispell-personal-dictionary (concat
                               (file-name-as-directory pure-dir-private)
                               "pure-dictionary.gpg"))
  ;; Alternate list of words used by ispell-lookup-word
  (ispell-alternate-dictionary (concat
                                (file-name-as-directory pure-dir-private)
                                "english-words.txt")))

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
              ("C-;" . flyspell-correct-word-before-point)
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
  ;; Try first local, the remote
  (dictionary-server "localhost")
  :bind
  (:map text-mode-map
        ("M-#" . dictionary-lookup-definition)))

;;;;; = abbrev - replace acronims with full word
;; acronims could also be spelling mistakes
;; TODO - not starting - 'abbrev-mode t'
(use-package abbrev
  :ensure nil
  :custom
  (abbrev-file-name pure-abbrev-defs)
  (save-abbrevs 'silently)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  :hook
  ;; Activate abbrev in text-mode and prog-mode
  ((text-mode prog-mode) . abbrev-mode))

;;;;; = dabbrev - dynamic abbriviations
;; expand word from other words in buffer or other buffers
;; Key bindings:
;; M-/   : dabbrev-expand
;; M-C-/ : dabbrev-complete
;; Can be used in conjunction with a loaded 'language' file, like oxford5000
(use-package dabbrev
  :disabled
  :ensure nil
  :bind
  ("M-/" . dabbrev-expand)
  ("M-C-/" . dabbrev-completion))

;;;;; = hippie-exp
;; Expand many things - other than words - like dabbrev
;; Key bindings:
;; M-/   : dabbrev-expand
(use-package hippie-exp
  :ensure nil
  :custom
  (hippie-expand-try-functions-list
   '(try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol
     try-expand-line))
  :bind
  ([remap dabbrev-expand] . hippie-expand))

;;;;; = delsel - delete selected
;; When a region is selected and delete or backspace pressed, delete selection.
(use-package delsel
  :ensure nil
  :hook
  (prog-mode . delete-selection-mode))

;;;; Navigation
;;;;; = imenu - list content of a buffer in headers
(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  (imenu-sort-function 'imenu--sort-by-name)
  :config
  (defun pure--imenu-use-package ()
    (add-to-list 'imenu-generic-expression
                 '("Used Packages:"
                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  :hook
  (emacs-lisp-mode . pure--imenu-use-package))

;;;; Programming
;;;;; = project - project management
(use-package project
  :ensure nil
  :custom
  ;; Place project files in the .cache
  (project-list-file (expand-file-name "projects" pure-dir-cache))
  ;; Show project information on the modeline
  (project-mode-line t))

;;;;; = vc - version control
(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-suppress-confirm t)
  (vc-command-messages t))

;;;;; = vc-git - git backend for version control
(use-package vc-git
  :ensure nil
  :after vc
  :custom
  (vc-git-diff-switches "--patch-with-stat")
  (vc-git-print-log-follow t))

;;;;; = vc-annotate - display version history of file in buffer
(use-package vc-annotate
  :ensure nil
  :after vc
  :custom
  (vc-annotate-display-mode 'fullscale))

;;;;; = outline - code folding
;; Navigate elisp files easily.
;; Alternative: allout-minor-mode
(use-package outline
  :ensure nil
  :preface
  (defun outline-show-level1 ()
    "Outline show level 1."
    (interactive)
    (outline--show-headings-up-to-level 1))
  (defun outline-show-level2 ()
    "Outline show level 2."
    (interactive)
    (outline--show-headings-up-to-level 2))
  (defun outline-show-level3 ()
    "Outline show level 3."
    (interactive)
    (outline--show-headings-up-to-level 3))
  (defun outline-show-level4 ()
    "Outline show level 4."
    (interactive)
    (outline--show-headings-up-to-level 4))
  :functions
  (outline-level
   outline-hide-sublevels
   outline--show-headings-up-to-level)
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-highlight 'override)
  :bind (:map outline-minor-mode-map
              ("<backtab>"  . outline-cycle-buffer)
              ("<tab>"      . outline-cycle)
              ("TAB"        . outline-cycle)
              ("M-<right>"  . outline-demote)
              ("M-<left>"   . outline-promote)
              ("M-<up>"     . outline-move-subtree-up)
              ("M-<down>"   . outline-move-subtree-down)
              ("M-<return>" . outline-inser-heading) ;graphics
              ("M-RET"      . outline-insert-heading) ;tty
              ("C-c C-n"    . outline-next-heading)
              ("C-c C-p"    . outline-previous-heading)
              ("M-1"        . outline-show-level1)
              ("M-2"        . outline-show-level2)
              ("M-3"        . outline-show-level3)
              ("M-4"        . outline-show-level4))
  :hook
  (emacs-lisp-mode . (lambda ()
                       (outline-minor-mode)
                       ;; prevent `outline-level' being overwritten by `lispy'
                       (setq-local outline-level #'outline-level)
                       ;; setup heading regexp specific to `emacs-lisp-mode'
                       (setq-local outline-regexp ";;;\\(;* \\)")
                       ;; heading alist allows for subtree-like folding
                       (setq-local outline-heading-alist
                                   '((";;; " . 1)
                                     (";;;; " . 2)
                                     (";;;;; " . 3)
                                     (";;;;;; " . 4)
                                     (";;;;;;; " . 5)))
                       (outline-hide-sublevels 3)))
  (sh-mode . (lambda ()
               (outline-minor-mode)
               ;; prevent `outline-level' being overwritten by `lispy'
               ;;(setq-local outline-level #'outline-level)
               ;; setup heading regexp specific to `emacs-lisp-mode'
               (setq-local outline-regexp "##\\(#* \\)")
               ;; heading alist allows for subtree-like folding
               (setq-local  outline-heading-alist
                            '(("## " . 1)
                              ("### " . 2)
                              ("#### " . 3)
                              ("##### " . 4)
                              ("###### " . 5)))
               (outline-hide-sublevels 1)))
  (python-mode . (lambda ()
                   (outline-minor-mode)
                   (setq-local outline-regexp " *\\(def \\|clas\\|#hea\\)")
                   (outline-hide-sublevels 1))))

;;;;; = hideshow - function and expressiona folding
;; Fold code blocks or expressions.
;; Useful for large code files that have no other narrowing assistance.
;; Executed with the unusual C-c @ ... keybindings.
(use-package hideshow
  :ensure nil
  :custom
  (hs-hide-comments-when-hiding all nil)
  (hs-isearch-open 'code)
  :hook
  (prog-mode . hs-minor-mode))

;;;;; = reveal - open folded blocks when searching
(use-package reveal
  :ensure nil
  :hook
  (prog-mode . reveal-mode))

;;;;; = elec-pair - parenthesis
;; Auto insert oposite parenthesis
(use-package elec-pair
  :ensure nil
  :hook
  ((text-mode prog-mode) . electric-pair-local-mode))

;;;;; = whitespace - show whitespaces
;; Show unnecessary whitespaces in selected major modes.
(use-package whitespace
  :ensure nil
  :hook
  ;; Remove training whitespaces, cleanup tabs, etc
  (before-save . whitespace-cleanup))

;;;;; = flymake - identify code faults
;; Error and warning code checking
(use-package flymake
  :ensure nil
  :functions
  (flymake-show-project-diagnostics
   flymake-show-buffer-diagnostics
   flymake--project-diagnostics-buffer
   flymake--diagnostics-buffer-name)
  :custom
  (flymake-show-diagnostics-at-end-of-line nil) ;; 'short (annoying)
  :config
  (defun pure--flymake-toggle-diagnostics-buffer ()
    "Toggle the diagnostics buffer when entering/exiting `flymake-mode'."
    (let* ((root (vc-root-dir))
           (command (if root
                        #'flymake-show-project-diagnostics
                      #'flymake-show-buffer-diagnostics))
           (window (get-buffer-window
                    (if root
                        (flymake--project-diagnostics-buffer root)
                      (flymake--diagnostics-buffer-name)))))
      (if flymake-mode
          (funcall command)
        (when (window-live-p window)
          (with-selected-window window
            (kill-buffer-and-window))))))
  :bind
  (:map flymake-mode-map
        ("M-p" . flymake-goto-previous-error)
        ("M-n" . flymake-goto-next-error))
  ;; :hook
  ;; (flymake-mode . pure--flymake-toggle-diagnostics-buffer))
  )

;;;; Treesit - Language installers and Mode Mappers
;;;;; = treesit - Emacs language parser
;; Parses structured / code files (files only) to provide better
;; color coding, folding, etc.
;; Languages to be installed separately with pure-treesit-install-and-remap.
(use-package treesit
  :ensure nil
  :demand t
  :when (pure-treesit-p)
  :defines
  (org-src-lang-modes)
  :functions
  (treesit-ready-p)
  :preface
  (defun pure-treesit-p ()
    "Check if Emacs was built with treesiter in a protable way."
    (and (fboundp 'treesit-available-p)
         (treesit-available-p)))

  (cl-defun pure-treesit-install-and-remap
      (lang url &key revision source-dir modes remap org-src)
    "Convenience function for installing and enabling a ts-* mode.

LANG is the language symbol.  URL is the Git repository URL for the
grammar.  REVISION is the Git tag or branch of the desired version,
defaulting to the latest default branch.  SOURCE-DIR is the relative
subdirectory in the repository in which the grammar’s parser.c file
resides, defaulting to \"src\".  MODES is a list of modes to remap to a
symbol REMAP.  ORG-SRC is a cons specifying a source code block language
name and a corresponding major mode."
    (when (and (fboundp 'treesit-available-p)
               (treesit-available-p))
      (unless (treesit-language-available-p lang)
        (add-to-list
         'treesit-language-source-alist
         (list lang url revision source-dir))
        (treesit-install-language-grammar lang))
      (when (and remap (treesit-ready-p lang))
        (dolist (mode modes)
          (add-to-list
           'major-mode-remap-alist
           (cons mode remap))))
      (when (and org-src (treesit-ready-p lang))
        (eval-after-load 'org
          (lambda ()
            (add-to-list 'org-src-lang-modes org-src))))))
  :custom
  (treesit-font-lock-level 2)
  (treesit-outline-predicate nil)
  (treesit-simple-imenu-settings t))

;;;;; = html - Treesit support for html
;; Replace html-mode, mhtml-mode and sgml-mode with html-ts-mode.
(use-package html-ts-mode
  :ensure nil
  :defer t
  :init
  (pure-treesit-install-and-remap
   'html
   "https://github.com/tree-sitter/tree-sitter-html"
   :revision "master"
   :source-dir "src"
   :modes '(mhtml-mode html-mode)
   :remap 'html-ts-mode
   :org-src '("html" . html-ts))
  :custom
  (html-ts-mode-indent-offset 4))

;;;;; = css - Treesit support for css
;; Replace css-mode with css-ts-mode.
;; Note: css-ts-mode is defined in `css-mode' package
(use-package css-mode
  :ensure nil
  :defer t
  :init
  (pure-treesit-install-and-remap
   'css
   "https://github.com/tree-sitter/tree-sitter-css"
   :revision "master"
   :source-dir "src"
   :modes '(css-mode)
   :remap 'css-ts-mode
   :org-src '("css" . css-ts))
  :custom
  (css-indent-offset 2))

;;;;; = js - Treesit support for java script
;; Replace js-mode with js-ts-mode.
;; Note: js-ts-mode is defined in the `js' package
(use-package js
  :ensure nil
  :defer t
  :init
  (pure-treesit-install-and-remap
   'javascript
   "https://github.com/tree-sitter/tree-sitter-javascript"
   :revision "master"
   :source-dir "src"
   :modes '(js-mode)
   :remap 'js-ts-mode
   :org-src '("js" . js-ts)))

;;;;; = typescript - Treesit support for typescript
;; Replace typescript-mode with typescript-ts-mode
(use-package typescript-ts-mode
  :ensure nil
  :defer t
  :init
  (pure-treesit-install-and-remap
   'typescript
   "https://github.com/tree-sitter/tree-sitter-typescript"
   :revision "master"
   :source-dir "typescript/src"
   :modes '(typescript-mode)
   :remap 'typescript-ts-mode
   :org-src '("typescript" . typescript-ts)))

;;;;; = tsx - Treesit support for tsx
;; Replace tsx-mode with tsx-ts-mode
(use-package typescript-ts-mode
  :ensure nil
  :defer t
  :init
  (pure-treesit-install-and-remap
   'tsx
   "https://github.com/tree-sitter/tree-sitter-typescript"
   :revision "master"
   :source-dir "tsx/src"
   :modes '(tsx-mode)
   :remap 'tsx-ts-mode
   :org-src '("tsx" . tsx-ts)))

;;;;; = yaml - Treesit support for yaml
;; There is no official `yaml-mode'
;; Note: Has no imenu and indentation support !?!
(use-package yaml-ts-mode
  :ensure nil
  :after (treesit)
  :defer t
  :init
  (pure-treesit-install-and-remap
   'yaml
   "https://github.com/ikatyang/tree-sitter-yaml"
   :revision "master"
   :source-dir "src"
   :modes '(yaml-mode)
   :remap 'yaml-ts-mode
   :org-src '("yaml" . yaml-ts)))

;;;;; = toml - Treesit support for toml
;; Replace the `conf-toml-mode' with `toml-ts-mode'
(use-package toml-ts-mode
  :ensure nil
  :after (treesit)
  :defer t
  :init
  (pure-treesit-install-and-remap
   'toml
   "https://github.com/tree-sitter/tree-sitter-toml"
   :revision "master"
   :source-dir "src"
   :modes '(conf-toml-mode)
   :remap 'toml-ts-mode
   :org-src '("toml" . toml-ts))
  :custom
  (toml-ts-mode-indent-offset 2))

;;;;; = json - Treesit support for `json'
;; Replace json-mode with `json-ts-mode'
(use-package json-ts-mode
  :ensure nil
  :defer t
  :after (json treesit)
  :init
  (pure-treesit-install-and-remap
   'json
   "https://github.com/tree-sitter/tree-sitter-json"
   :revision "master"
   :source-dir "src"
   :modes '(js-json-mode)
   :remap 'json-ts-mode
   :org-src '("json" . json-ts))
  :custom
  (json-ts-mode-indent-offset 2))

;;;;; = python - Treesit support for python
;; Replace python-mode with `python-ts-mode'.
;; Note: python-ts-mode is defined in the `python' package
(use-package python
  :ensure nil
  :after (treesit)
  :defer t
  :init
  (pure-treesit-install-and-remap
   'python
   "https://github.com/tree-sitter/tree-sitter-python"
   :revision "master"
   :source-dir "src"
   :modes '(python-mode)
   :remap 'python-ts-mode
   :org-src '("python" . python-ts))
  :custom
  (python-indent-offset 4))

;;;;; = eglot - Emacs client for the Language Server Protocol
;; Note: Supports a single language per buffer only (2025-01)
;; Manual installation for language servers required.
;; Python: pacman -S python-lsp-server
(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("C-c i" . eglot)
              ("C-c r" . eglot-rename)
              ("C-c f" . eglot-format-buffer))
  :hook
  (python-ts-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:colorProvider
     ;; :documentHighlightProvider
     ;; :documentFormattingProvider
     ;; :documentRangeFormattingProvider
     ;; :documentOnTypeFormattingProvider
     ;; :foldingRangeProvider
     :hoverProvider)))

;;;; Programming Languages

;;;;; = kmacro - Emacs built-in macro mechansim
;; F3 - To start recording a macro
;; F4 - To end recording a macro
;; F4 - To replay the recorded macro
;; F1 - To name the macro
;; F2 - Load one of the named macro's
;; Note that named macros can also be called with M-x 'macroname'
(use-package kmacro
  :ensure nil
  :config
  (defun kmacro-load-macro (kmacro-sym)
    "Load a macro that that has been defined as a kmacro
    ;; To insert new macros
    ;; 1. Record the macro, start with F3 and end with F4
    ;; 2. Name the macro, C-x C-k n
    ;; 3. Save the macro in a buffer, M-x insert-kbd-macro"
    (interactive
     (list (intern
            (completing-read
             "Kmacro name: "
             (let ((result))
               (mapatoms (lambda (it)
                           (when (kmacro-p (symbol-function it))
                             (push it result))))
               result)))))
    (setq last-kbd-macro (kmacro--keys (symbol-function kmacro-sym))))
  :bind
  ("<f1>" . kmacro-name-last-macro)
  ("<f2>" . kmacro-load-macro))

;;;; Shells

;;;;; = eshell - the Emacs shell created with elsip. Runs everywhere.
(use-package eshell
  :ensure nil
  :custom
  (eshell-directory-name (concat pure-dir-cache "eshell"))
  :init
  ;; Open a dedicated terminal for the following line based sub commands
  ;; (add-to-list 'eshell-visual-options '("git" "--paginate" "--help"))
  ;; (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show")))
  )

;;;; Tools

;;;;; = calendar - calendar dialogue.
(use-package calendar
  :ensure nil
  :custom
  ;; Weeks start on Monday
  calendar-week-start-day 1)

;;;;; = url
;; Used for most web related packages.

;; Some bug in Emacs 30 doesn't load this variable.
(defcustom url-configuration-directory
  (locate-user-emacs-file "url/" ".url/")
  "Directory used by the URL package for cookies, history, etc."
  :type 'directory
  :group 'url)

(use-package url
  :ensure nil
  :custom
  (url-configuration-directory (concat pure-dir-cache "url/")))

;;;; Security and Privacy

;;;;; = epg - Emacs GnuPG interface
;; add .gpg to any file and it will automatically by encrypted/decrypted
(use-package epg
  :ensure nil
  :custom
  ;; Use version 2 of OpenGP
  (epg-gpg-program "gpg2")
  ;; Use emacs to enter passphrases instead of pinentry.
  (epg-pinentry-mode 'loopback))

;;;;; = epa - EasyPG file encryption and decryption
;; add .gpg to any file and it will automatically by encrypted/decrypted
(use-package epa
  :custom
  (epa-pinentry-mode 'loopback)
  :hook
  ;; Automatically encrypt / decrypt .gpg files
  (after-init . epa-file-enable))

;;;;; = auth-source - handle username / passwords for accounts
;; provides encryption and decryption
;; -  a gpg key is required
;; - ~/.authinfo.gpg
;; format: machine HOST login NAME password VALUE port NUMBER
;; example: machine imap.gmail.com login john_doe@gmail.com password "*secret*"
(use-package auth-source
  :ensure nil
  :custom
  ;; Files containing the secrets
  (auth-sources (list "~/.authinfo.gpg"
                      "secrets:echjansen"))
  ;; Support debugging during authorization. Should be turned off.
  (auth-source-debug nil)
  :init
  (defun pure-password-lookup (&rest keys)
    "Return the password of any field provided by KEYS.
Requires a ~./authinfo.gpg file containing the entries."
    (let ((result (apply #'auth-source-search keys)))
      (if result
          (funcall (plist-get (car result) :secret))
        nil))))

;;;;; = auto-source-pass
;; Add the GNUS pass-store to the auth-sources variable
;; Use:
;; (auth-source-pass-get 'secret "email/mail1")
;; (auth-source-pass-get "email" "email/mail1")
(use-package auth-source-pass
  :ensure nil
  :hook
  (after-init . auth-source-pass-enable))

;;;; Communication

;;;;; = eww - Emacs web wowser
(use-package eww
  :ensure nil
  :custom
  (url-configuration-directory (concat pure-dir-cache "eww/"))
  (url-cookie-file (concat url-configuration-directory "cookies"))
  :bind
  ("C-c c w" . eww))

;;;;; = erc - Internet relay chat
(use-package erc
  :ensure nil
  :custom
  (erc-server "irc.libera.chat")
  (erc-nick "pure-emacs")
  (erc-user-full-name "Pure Emacs")
  (erc-track-shorten-start 8)
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#guix")))
  (erc-kill-buffer-on-part t)
  (erc-auto-query 'bury)
  :bind
  ("C-c i" . erc-tls))

;;;; Org Mode

;;;;; = org-mode - the one and only writing environment (and more)
(use-package emacs
  :ensure nil
  :custom
  ;; Appearance
  (org-indent-mode t "Allign text with header virtually")
  (org-hide-leading-stars t "Only one star per header")
  (org-cycle-separator-lines 1 "No empty lines in collapsed view")
  (org-ellipsis " ..▼" "nicer elipses ↷ ↴ ▼")
  (org-fontify-quote-and-verse-blocks t "Make quotes stand out")
  (org-image-actual-width  500 "Show images at 500px")
  (org-insert-heading-respect-content t "Insert new headings after subtree")
  (org-list-allow-alphabetical t "Allow alphabetical list")
  (org-read-date-prefer-future 'time "Incomplete dates refer to future")
  (org-startup-folded t "Start org in outline")
  (org-startup-indented t "Start with indentation of headlines")
  (org-auto-align-tags t "Auto-align tags")
  (org-tags-column 60 "Place tags directly next to headline text")

  ;; Lists
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (org-list-indent-offset 1 "Increase sub-item indentation")

  ;; States and logging
  (org-use-fast-todo-selection t "Allow moving to any state")
  ;; ! timestamped states
  ;; @ timestamped notes
  (org-todo-keywords  '((sequence "TODO(t!)"
                                  "ACTIVE(a!)"
                                  "|"
                                  "DONE(d!)")
                        (sequence "WAITING(w@/!)"
                                  "HOLD(h@/!)"
                                  "|"
                                  "CANCELED(c@/!)"
                                  "PHONE"
                                  "MEETING")))
  (org-todo-keyword-faces '(("TODO" :foreground "yellow" :weight bold)
                            ("ACTIVE" :foreground "white" :weight bold)
                            ("DONE" :foreground "forest green" :weight bold)
                            ("WAITING" :foreground "orange" :weight bold)
                            ("HOLD" :foreground "magenta" :weight bold)
                            ("CANCELLED" :foreground "forest green" :weight bold)
                            ("MEETING" :foreground "forest green" :weight bold)
                            ("PHONE" :foreground "forest green" :weight bold)))
  ;;(org-log-done 'time "Record when task moves to DONE state")
  (org-log-into-drawer nil "Record in LOGBOOK drawer")
  (org-log-state-notes-insert-after-drawers nil)
  (org-log-redeadline 'note "Add time and note for deadline changes")
  (org-log-reschedule 'note "Add time and note for rescheduling tasks")

  ;; Movement
  (org-return-follows-link t "Make RET follow links")
  (org-special-ctrl-a/e t "Better movement in headers")

  ;; Searching
  (org-imenu-depth 8 "Scan to depth 8 w/imenu")
  (imenu-auto-rescan t "Make sure imenu refreshes")

  ;; Source block settings
  (org-src-fontify-natively t "Use lang-specific fontification")
  (org-src-window-setup 'other-window "Edit source in other window")
  (org-src-tab-acts-natively t "Use lang bindings")

  (org-confirm-babel-evaluate t "Confirm evaluation")

  ;; TODOS
  (org-use-fast-todo-selection 'expert "on't use popup window for todos")
  (org-enforce-todo-dependencies t "Don't set DONE when children are not")
  (org-enforce-todo-checkbox-dependencies t "Don't set DONE when check's open")

  ;; Files and agenda
  (org-directory pure-dir-notes "directory with orgfiles")
  (org-agenda-files (list org-directory) "files with todo's")

  (org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
           ("WAITING" ("WAITING" . t))
           ("HOLD" ("WAITING") ("HOLD" . t))
           (done ("WAITING") ("HOLD"))
           ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
           ("ACTIVE" ("WAITING") ("CANCELLED") ("HOLD"))
           ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))))

;;;; Pure Functions
;;;;; = pure--suppress-messages
(defun pure--suppress-messages (func &rest args)
  "Suppress message output from FUNC ith ARGS."
  ;; Some packages are too noisy.
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply func args)
      (advice-remove 'message #'silence))))

;; Suppress noise in the Message buffer
(advice-add 'recentf-cleanup :around #'pure--suppress-messages)
(advice-add 'recentf-load-list :around #'pure--suppress-messages)
(advice-add 'repeat-mode :around #'pure--suppress-messages)

(provide 'pure-emacs)
;;; pure-emacs.el ends here
