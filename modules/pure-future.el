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
;;;;; = vertico - VERTical Interactive COmpletion
;; Current version on Melpa has an issue with compiling.
;; Not using fido-vertical-mode as it does not respect orderless.
(use-package vertico
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

;;;;; = corfu-history - display used candidates first
(use-package corfu-history
  :ensure nil
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history)
  :hook
  (prog-mode . corfu-history-mode))

;;;;; = corfu-terminal - tty support for corfu
(use-package corfu-terminal
  :unless (display-graphic-p)
  :hook
  (prog-mode . corfu-terminal-mode))

;;;;; = embark = emacs mini-buffer actions rooted in keymaps
;; Like a right mmouse click
;; Disabled as it interferes with marking and echo in the buffer.
(use-package embark-consult
  :disabled
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

;;;; Coding and Programming Languages
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


;;;; Shells

;;;; Tools
;;;;; = magit - git porcelain inside emacs
;; Git interface
(use-package magit
  :commands
  (magit-status))

;;;;; = consult - improved functions
;; Loading all consult functions in case they come in handy
(use-package consult
  :config
  (require 'consult-imenu nil t)
  (require 'consult-flymake nil t)
  :bind
  ("C-x C-b" . consult-buffer)          ; Replacing ibuffer
  ("M-g i"   . consult-imenu))          ; Replacing imenu

;;;; Security and Privacy

;;;; Communication
;;;; Org Mode
;;;; Pure Functions

;;; Provide
(provide 'pure-future)
;;; pure-future.el ends here
