;;; init.el --- Pure Emacs initialization. -*- lexical-binding: t -*-

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
;; - Fast startup time - achieved with byte compiling and use-package.
;; - Emacs build in features - unless inferior or non existing.
;; - Package.el package manager - has all features required (including vc)

;;; Code:

;; Add the `modules' directory to the load path
(add-to-list 'load-path
             (concat (file-name-as-directory user-emacs-directory) "modules"))

;; Load the pure-emacs common configuration.
(require 'pure-common nil t)

;; Load the pure-emacs configuration (all built-in features)
(require 'pure-emacs nil t)

;; Load the pure-future configuration (installed features)
(require 'pure-future nil t)

;; Load the pure-email configuration (optional, requires ~mu~)
;; (require 'pure-email nil t)

(provide 'init)
;;; init.el ends here
