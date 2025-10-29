;;; pure-coding.el --- Software coding  -*- lexical-binding: t -*-

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

;; This module is dedicated to software and web development
;; The following tools are implemented:
;; - lsp-mode       ; LSP client replacment for eglot
;; - lsp-ui         ; lsp-mode additional UI features
;; - lsp-ui-imenu   ; Nice imenu
;; - ox-hugo        ; Export org to hugu markup files
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

;;;; IDE - Appearance

;;;;; = indent-bars - bars driven by treesit
(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  (indent-bars-treesit-wrap '((python
                               argument_list parameters ; for python, as an example
                               list list_comprehension
                               dictionary dictionary_comprehension
                               parenthesized_expression subscript)))
  :config
  (require 'indent-bars-ts)
  :hook ((python-base-mode
          yaml-mode) . indent-bars-mode))

;;;;; = outline-indent - code folding based on indentation levels
;; Also does shifting in and out, moving up and down, etc.
(use-package outline-indent
  :custom
  (outline-indent-ellipsis " ▼ ")
  :hook
  (((python-base-mode yaml-mode) . outline-indent-minor-mode)
   (python-base-mode . (lambda ()
                         (setq-local outline-indent-default-offset 4)
                         (setq-local outline-indent-shift-width 4)
                         (setq-local outline-blank-line t)))
   (yaml-ts-mode     . (lambda ()
                         (setq-local outline-indent-default-offset 2)
                         (setq-local outline-indent-shift-width 2)))))

;;;; IDE - Editing
;;;;; = expand-region - select region
(use-package expand-region
  :bind
  ("C->" . er/expand-region)
  ("C-<" . er/contract-region))

;;;;; = change-inner - vim like chane inner / outer
(use-package change-inner
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer))

;;;; IDE - Language Server Protocol tools
;;;;; = lsp-mode - replacement for eglot
;; Although eglot is included in Emacs since version 29, lsp-mode configured
;; here has many IDE like features that are missing in eglot.
;; Choose the LSP environment best suitable for your current project.
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-server-install-dir (expand-file-name "lsp" pure-dir-cache))
  (lsp-session-file (expand-file-name ".lsp-session-v1" pure-dir-cache))
  ;; Integrations with other services
  (lsp-completion-provider :none)       ; Corfu is capf
  (lsp-diagnostics-provider :flymake)   ; Flymake fallback
  ;; Mode line configuration
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-workspace-status-enable t)
  (lsp-modeline-diagnostics-scope :workspace)
  ;; Header line breadcrumb configuration
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  ;; Other features
  (lsp-lens-enable t)
  (lsp-semantic-tokens-enable t)
  (lsp-enable-snippet nil)              ; Not using yasnippet
  (lsp-eldoc-enable-hover nil)
  :config
  (require 'lsp-diagnostics nil t)
  (require 'lsp-lens nil t)
  (require 'lsp-modeline nil t)
  (require 'lsp-headerline nil t)
  :hook
  ((lsp-mode         . lsp-enable-which-key-integration)))

;;;;; = lsp-ui - Additional UI features
(use-package lsp-ui
  :after lsp
  :demand t)

;;;;; = lsp-ui-imenu - Improved UI imenu
(use-package lsp-ui-imenu
  :ensure nil
  :commands (lsp-ui-imenu)
  :custom
  ;; lsp-imenu configuration
  (lsp-ui-imenu-buffer-name "Symbols")
  (lsp-ui-imenu-auto-refresh t))

;;;;; = combobulate - code manipulation
(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate")
  :custom
  (combobulate-key-prefix "C-c o")
  :hook
  ((treesit-inspect-mode-hook . combobulate-mode)))

;;;; IDE - Debugging
;;;;; = dape - Debugging Adapter Protocol for Emacs
;; Key-bindings accessed via C-x C-a (with repeat functionality)
;; Dape requires packages to be installed for program languages
;; python - pacman -S python-debugpy (or, pip install debugpy)
(use-package dape
  :commands
  (dape)
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-inlay-hints t)
  :config
  (add-to-list 'dape-configs
               '(python
                 :modes (python-ts-mode)
                 :command "python"
                 :command-args ("-m" "debugpy.adapter" "--host" "127.0.0.1" "--port" :autoport)
                 :port :autoport
                 :type "python"
                 :request "launch"
                 ;;                 :program dape-program-name ;; This will typically be the current buffer's file
                 :cwd dape-cwd ;; Current working directory
                 :args nil ;; Arguments to pass to your Python script, e.g., '("arg1" "arg2")
                 :justMyCode nil ;; Set to t to only debug your code, skipping library code
                 :console "integratedTerminal" ;; Or "internalConsole"
                 :showReturnValue t
                 :stopOnEntry nil))
  :hook
  (dape-mode    . repeat-mode)
  (dape-mode    . eldoc-mode)
  (kill-emacs   . dape-breakpoint-save)
  (after-init   . dape-breakpoint-load)
  (dape-compile . kill-buffer))

;;;; Programming languages
;;;;; = Python
;;;;;; = python mode
;; Configure the IDE for python development
;; - Language Server (eglot with treesit)
;; - Auto completion (corfu)
;; - No line folding
;; Packages to install
;; - pacman -S pyright (python code checking)
(use-package python
  :ensure nil
  :mode
  ("\\.py\\'" . python-ts-mode)
  :custom
  (python-interpreter "~/.python/venv/bin/python")
  (python-shell-interpreter "~/.python/venv/bin/python")
  :bind
  (:map python-ts-mode-map
        (("<f5>"  . recompile)
         ("<f6>"  . eglot-format-buffer)))
  :hook
  ((python-ts-mode . eglot-ensure)
   (python-ts-mode . corfu-mode)
   (python-ts-mode . toggle-truncate-lines)))

;;;;;; = pyvenv
;; handles python virtual environments automatically
;; - ensure you have a git repository as 'project' relies on it
;; - created in your project folder with:
;;   python -m venv .venv
;;   pip install python-lsp-server[[all]]
(use-package pyvenv
  :commands (pyvenv-workon              ; Activate project from $WORKON_HOME
             pyvenv-activate            ; Activate local project
             pyvenv-deactivate)         ; De-activate any project
  :config
  (defun pure-pyvenv-auto-activate ()
    "Try to activate a .venv in the project root automatically."
    (interactive)
    ;; Check if .venv exists in the project root and activate it
    (when (file-directory-p (concat (project-root (project-current)) ".venv"))
      (pyvenv-activate (concat (project-root (project-current)) ".venv"))))
  :hook
  ((python-mode python-ts-mode) . pure-pyvenv-auto-activate))

;;;;; = Shell (bash)
;;;;;; = shell mode
;; Determine what features to turn on when a shell script is opened
;; - pacmna -S shellcheck
(use-package emacs
  :ensure nil
  :custom
  (sh-shellcheck-program "shellcheck")
  :mode
  ("\\.sh\\'" . bash-ts-mode)
  :hook
  ((bash-ts-mode . eglot-ensure)
   (bash-ts-mode . corfu-mode)))

;;;; Org mode extensions to support Hugo (org to md)
;;;;; = ox-hugo - export from org to md files
(use-package ox-hugo
  :after ox
  :demand t)

;;;;; = ox-hugo-auto-export-mode
(use-package org-hugo-auto-export-mode
  :ensure nil
  :after ox-hugo
  :demand t)

;;; Provide:
(provide 'pure-coding)

;;; pure-coding.el ends here
