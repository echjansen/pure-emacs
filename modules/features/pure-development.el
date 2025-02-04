;;; pure-development.el --- Web development  -*- lexical-binding: t -*-

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

;;;; IDE - Language Server Protocol tools
;;;;; = lsp-mode - replacement for eglot
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
  (use-package lsp-diagnostics :ensure nil
    :commands lsp-diagnostics-mode)
  (use-package lsp-lens :ensure nil
    :commands lsp-lens--enable)
  (use-package lsp-modeline :ensure nil
    :commands lsp-modeline-workspace-status-mode)
  (use-package lsp-headerline :ensure nil
    :commands lsp-headerline-breadcrumb-mode)
  (setq read-process-output-max 16384)
  :hook
  (((python-ts-mode) . lsp-deferred)
   (lsp-mode         . lsp-enable-which-key-integration)))

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
(provide 'pure-development)

;;; pure-development.el ends here
