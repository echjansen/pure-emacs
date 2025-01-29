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
;;;; Treesit Extensions - Language installers and Mode Mappers
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

;;;;; - json - Treesit support for `json'
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

;;;;; = combobulate - code manipulation
(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate")
  :custom
  (combobulate-key-prefix "C-c o")
  :hook
  ((prog-mode . combobulate-mode)))

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
