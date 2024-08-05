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

;;;;; = marginalia - display minibuffer meta data
;; Add annotations to the completion buffer
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-t" . marginalia-cycle))
  :hook
  (minibuffer-setup . marginalia-mode))

;;;;; = vundo - graphical undo tree
(use-package vundo
  :commands vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :bind
  ("C-x u" . vundo))

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
  ;;(corfu-quit-no-match 'seperator)
  (corfu-quit-no-match nil)
  :hook ((prog-mode . corfu-mode)
         (text-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)))

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
  (corfu-mode . corfu-terminal-mode))

;;;;; = cape - completion at point extensions
(use-package cape
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
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
  (defun pure--cape-setup-elisp ()
    (setf (elt (cl-member 'elisp-completion-at-point
                          completion-at-point-functions) 0)
          #'elisp-completion-at-point)
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
    (add-to-list 'completion-at-point-functions #'cape-file))

  (defun pure--cape-setup-org ()
    (add-to-list 'completion-at-point-functions #'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block))

  :hook
  ;; Register the functions via hooks so one has control over the
  ;; the lookup functions depending on the mode.
  ((emacs-lisp-mode . pure--cape-setup-elisp)
   (org-mode        . pure--cape-setup-org)))

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
;;;; = magit - git porcelain inside emacs
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
