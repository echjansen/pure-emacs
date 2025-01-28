;;; pure-dev-web.el --- Web development  -*- lexical-binding: t -*-

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

;; This module is useful when you are developing and maintaining content
;; for websites.
;; This module further contains opinionated modules to manage Hugo websites.
                                        ;
;;; Code:

;;;;
;;;; = treesit - Language installers and mode mappers
;;;;; = html - Treesit support for html
;; Replaces html-mode and mhtml-mode.
(use-package html
  :ensure nil
  :defer t
  :when (pure-treesit-p)
  :init
  (pure-treesit-install-and-remap
   'html "https://github.com/tree-sitter/tree-sitter-html"
   :revision "master"
   :source-dir "src"
   :modes '(mhtml-mode html-mode)
   :remap 'html-ts-mode
   :org-src '("html" . html-ts)))

;;;;; = css - Treesit support for css
;; Replaces css-mode.
(use-package css
  :ensure nil
  :defer t
  :when (pure-treesit-p)
  :init
  (pure-treesit-install-and-remap
   'css "https://github.com/tree-sitter/tree-sitter-css"
   :revision "v0.23.2"
   :source-dir "src"
   :modes '(css-mode)
   :remap 'css-ts-mode
   :org-src '("css" . css-ts)))

;;;; Org mode extensions to support Hugo (org to md)
;;;;; = ox-hugo - export from org to md files
(use-package ox-hugo
  :after ox
  :demand t)

;;;;; = ox-hugo-auto-export-mode
(use-package org-hugo-auto-export-mode
  :after ox-hugo
  :demand t)

;;; Provide:
(provide 'pure-dev-web)

;;; pure-dev-web.el ends here
