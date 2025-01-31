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
;; - ox-hugo        ; Export org to hugu markup files
                                        ;
;;; Code:

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
