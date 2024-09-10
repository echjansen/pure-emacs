;;; pure-line.el --- A custom status line  -*- lexical-binding: t -*-

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

;; Based on the excellent work from mclearc's lambda-line, pure-line is a
;; minimal, though opinionated, status-line (i.e. in Emacs the
;; information display either in the mode-line and/or header-line) for use as
;; either header or footer in a buffer.  The structure of the status-line takes
;; the following form: [ status | name (primary) tertiary | secondary ]

;; Usage: M-x pure-line-mode

;;; Code:

(require 'face-remap)
(require 'cl-lib)

;;;; Group

(defgroup pure-line nil
  "Pure-line group."
  :group 'mode-line
  :link '(url-link :tag "Homepage" "https://github.com/echjansen/pure-emacs"))

;;;; Custom Variable Settings

(defcustom pure-line-window-width-limit 0.25
  "The limit of the window width.
If `window-width' is smaller than the limit, some information won't be
displayed.  It can be an integer or a float number.  'nil' means no limit."
  :type '(choice integer
                 float
                 (const :tag "Disable" nil))
  :group 'pure-line)

(defcustom pure-line-position 'top
  "Default modeline position (top or bottom)."
  :type '(choice
          (const :tag "Nil"    nil)
          (const :tag "Top"    top)
          (const :tag "Bottom" bottom))
  :group 'pure-line)

(defcustom pure-line-prefix t
  "Include a prefix icon to indicate buffer status in the status-line."
  :type 'boolean
  :group 'pure-line)

(defcustom pure-line-prefix-padding t
  "Include prefix padding."
  :type 'boolean
  :group 'pure-line)

(defcustom pure-line-user-mode nil
  "User supplied mode to be evaluated for modeline."
  :type '(choice (const nil) function)
  :group 'pure-line)

(defcustom pure-line-abbrev nil
  "If t then show abbreviated mode symbol in modeline.
Default is nil.  To change the values of the major-mode symbols
see the value of `pure-line-abbrev-alist'"
  :group 'pure-line
  :type 'boolean)

(defcustom pure-line-git-diff-mode-line t
  "If t then show diff lines in modeline."
  :group 'pure-line
  :type 'boolean)

(defcustom pure-line-vc-symbol " 󰊢 "
  "Symbol to use in buffers visiting files under version control."
  :group 'pure-line
  :type 'string)

;; Visual Bell
(defcustom pure-line-visual-bell t
  "If t then use `pure-line-visual-bell'."
  :group 'pure-line
  :type 'boolean)

;; Invert status faces
;; This make pure-line look more like nano-modeline
(defcustom pure-line-status-invert nil
  "If 't' then invert the colors to get a box effect for the corner
of the status line."
  :group 'pure-line
  :type 'boolean)

;; Mode line symbols
(defcustom pure-line-gui-ro-symbol "  "
  "Modeline gui read-only symbol."
  :group 'pure-line
  :type 'string)

(defcustom pure-line-gui-mod-symbol "  "
  "Modeline gui modified symbol."
  :group 'pure-line
  :type 'string)

(defcustom pure-line-gui-rw-symbol "  "
  "Modeline gui read-write symbol."
  :group 'pure-line
  :type 'string)

(defcustom pure-line-tty-ro-symbol "  "
  "Modeline tty read-only symbol."
  :group 'pure-line
  :type 'string)

(defcustom pure-line-tty-mod-symbol "  "
  "Modeline tty read-only symbol."
  :group 'pure-line
  :type 'string)

(defcustom pure-line-tty-rw-symbol "  "
  "Modeline tty read-write symbol."
  :group 'pure-line
  :type 'string)

(defcustom pure-line-truncate-value 30
  "Value of modeline truncate-length function."
  :group 'pure-line
  :type 'integer)

(defcustom pure-line-hspace " "
  "Space adjustment for right end of modeline."
  :type 'string
  :group 'pure-line)

(defcustom pure-line-space-top +.35
  "Space adjustment for top of status-line.
Positive is upwards"
  :type 'float
  :group 'pure-line)

(defcustom pure-line-space-bottom -.5
  "Space adjustment for bottom of status-line.
Negative is downwards."
  :type 'float
  :group 'pure-line)

(defcustom pure-line-symbol-position .067
  "Space adjustment for symbol in status-line.
Negative is downwards."
  :type 'float
  :group 'pure-line)

(defcustom pure-line-syntax t
  "Show flycheck/flymake report in status-line."
  :type 'boolean
  :group 'pure-line)

(defcustom pure-line-which-func nil
  "Show `which-function-mode' display in status-line."
  :type 'boolean
  :group 'pure-line)

(defcustom pure-line-flycheck-label "Issues: "
  "Show with flycheck/flymake issues count."
  :type 'string
  :group 'pure-line)

(defcustom pure-line-icon-time nil
  "When set to non-nil show the time as an icon clock.
Time info is only shown `display-time-mode' is non-nil"
  :type 'boolean
  :group 'pure-line)

(defcustom pure-line-time-day-and-date-format "  %H:%M %Y-%m-%d "
  "`format-time-string'."
  :type 'string
  :group 'pure-line)

(defcustom pure-line-time-format "  %H:%M "
  "`format-time-string'."
  :type 'string
  :group 'pure-line)

(defcustom pure-line-display-group-start "[ "
  "Modeline display group start indicator."
  :group 'pure-line
  :type 'string)

(defcustom pure-line-display-group-end " ]"
  "Modeline display group end indicator."
  :group 'pure-line
  :type 'string)

(defcustom pure-line-mode-formats
  '(;; with :mode-p first
    (imenu-list-mode        :mode-p pure-line-imenu-list-mode-p
                            :format pure-line-imenu-list-mode)
    (org-capture-mode       :mode-p pure-line-org-capture-mode-p
                            :format pure-line-org-capture-mode
                            :on-activate pure-line-org-capture-activate
                            :on-deactivate pure-line-org-capture-deactivate)
    (prog-mode              :mode-p pure-line-prog-mode-p
                            :format pure-line-prog-mode
                            :on-activate pure-line-prog-activate
                            :on-deactivate pure-line-prog-deactivate)
    (mu4e-dashboard-mode    :mode-p pure-line-mu4e-dashboard-mode-p
                            :format pure-line-mu4e-dashboard-mode)
    (messages-mode          :mode-p pure-line-messages-mode-p
                            :format pure-line-messages-mode)
    (message-mode           :mode-p pure-line-message-mode-p
                            :format pure-line-message-mode)
    (term-mode              :mode-p pure-line-term-mode-p
                            :format pure-line-term-mode)
    (vterm-mode             :mode-p pure-line-vterm-mode-p
                            :format pure-line-term-mode)
    (eshell-mode            :mode-p pure-line-eshell-mode-p
                            :format pure-line-eshell-mode)
    (buffer-menu-mode       :mode-p pure-line-buffer-menu-mode-p
                            :format pure-line-buffer-menu-mode
                            :on-activate pure-line-buffer-menu-activate
                            :on-deactivate pure-line-buffer-menu-deactivate)
    (calendar-mode          :mode-p pure-line-calendar-mode-p
                            :format pure-line-calendar-mode
                            :on-activate pure-line-calendar-activate
                            :on-deactivate pure-line-calendar-deactivate)
    (completion-list-mode   :mode-p pure-line-completion-list-mode-p
                            :format pure-line-completion-list-mode)
    (deft-mode              :mode-p pure-line-deft-mode-p
                            :format pure-line-deft-mode)
    (doc-view-mode          :mode-p pure-line-doc-view-mode-p
                            :format pure-line-doc-view-mode)
    (elfeed-search-mode     :mode-p pure-line-elfeed-search-mode-p
                            :format pure-line-elfeed-search-mode
                            :on-activate pure-line-elfeed-search-activate
                            :on-deactivate pure-line-elfeed-search-deactivate)
    (elfeed-show-mode       :mode-p pure-line-elfeed-show-mode-p
                            :format pure-line-elfeed-show-mode)
    (elpher-mode            :mode-p pure-line-elpher-mode-p
                            :format pure-line-elpher-mode
                            :on-activate pure-line-elpher-activate)
    (help-mode              :mode-p pure-line-help-mode-p
                            :format pure-line-help-mode)
    (helpful-mode           :mode-p pure-line-helpful-mode-p
                            :format pure-line-help-mode)
    (info-mode              :mode-p pure-line-info-mode-p
                            :format pure-line-info-mode
                            :on-activate pure-line-info-activate
                            :on-deactivate pure-line-info-deactivate)
    (magit-mode             :mode-p pure-line-magit-mode-p
                            :format pure-line-magit-mode)
    (mu4e-compose-mode      :mode-p pure-line-mu4e-compose-mode-p
                            :format pure-line-mu4e-compose-mode)
    (mu4e-headers-mode      :mode-p pure-line-mu4e-headers-mode-p
                            :format pure-line-mu4e-headers-mode)
    (mu4e-loading-mode      :mode-p pure-line-mu4e-loading-mode-p
                            :format pure-line-mu4e-loading-mode)
    (mu4e-main-mode         :mode-p pure-line-mu4e-main-mode-p
                            :format pure-line-mu4e-main-mode)
    (mu4e-view-mode         :mode-p pure-line-mu4e-view-mode-p
                            :format pure-line-mu4e-view-mode)
    (org-agenda-mode        :mode-p pure-line-org-agenda-mode-p
                            :format pure-line-org-agenda-mode)

    (org-clock-mode         :mode-p pure-line-org-clock-mode-p
                            :format pure-line-org-clock-mode
                            :on-activate pure-line-org-clock-activate
                            :on-deactivate pure-line-org-clock-deactivate)
    (pdf-view-mode          :mode-p pure-line-pdf-view-mode-p
                            :format pure-line-pdf-view-mode)
    (fundamental-mode       :mode-p pure-line-fundamental-mode-p
                            :format pure-line-fundamental-mode)
    (text-mode              :mode-p pure-line-text-mode-p
                            :format pure-line-text-mode)

    ;; hooks only go last
    (ein-notebook-mode      :on-activate pure-line-ein-notebook-activate
                            :on-deactivate pure-line-ein-notebook-deactivate)
    (esh-mode               :on-activate pure-line-esh-activate
                            :on-deactivate pure-line-esh-deactivate)
    (ispell-mode            :on-activate pure-line-ispell-activate
                            :on-deactivate pure-line-ispell-deactivate)
    (mu4e-mode              :on-activate pure-line-mu4e-activate
                            :on-deactivate pure-line-mu4e-deactivate))

  "Modes to be evalued for modeline.
KEY mode name, for reference only. Easier to do lookups and/or replacements.
:MODE-P the function to check if :FORMAT needs to be used, first one wins.
:ON-ACTIVATE and :ON-DEACTIVATE do hook magic on enabling/disabling the mode.
"
  :type '(alist :key-type symbol
                :value-type (plist :key-type (choice (const :mode-p)
                                                     (const :format)
                                                     (const :on-activate)
                                                     (const :on-deactivate))
                                   :value-type function))
  :group 'pure-line)

(defcustom pure-line-mode-format-activate-hook nil
  "Add hooks on activation of the mode.
This is for those modes that define their own status-line."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'pure-line)

(defcustom pure-line-mode-format-deactivate-hook nil
  "Remove hooks on de-activation of the mode.
This is for those modes that define their own status-line."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'pure-line)

(defcustom pure-line-default-mode-format 'pure-line-default-mode
  "Default mode to evaluate.
This is if no match could be found in `pure-lines-mode-formats'"
  :type 'function
  :group 'pure-line)

;;;; Faces
;;;;; Line Faces

(defface pure-line-active
  '((t (:inherit (mode-line))))
  "Modeline face for active modeline."
  :group 'pure-line-active)

(defface pure-line-inactive
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive line."
  :group 'pure-line-inactive)

(defface pure-line-hspace-active
  '((t (:invisible t :family "Monospace" :inherit (mode-line))))
  "Face for vertical spacer in active line.")

(defface pure-line-hspace-inactive
  '((t (:invisible t :family "Monospace" :inherit (mode-line-inactive))))
  "Face for vertical spacer in inactive line.")

(defface pure-line-active-name
  '((t (:inherit (mode-line))))
  "Modeline face for active name element."
  :group 'pure-line-active)

(defface pure-line-inactive-name
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive name element."
  :group 'pure-line-inactive)

(defface pure-line-active-primary
  '((t (:weight light :inherit (mode-line))))
  "Modeline face for active primary element."
  :group 'pure-line-active)

(defface pure-line-inactive-primary
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive primary element."
  :group 'pure-line-inactive)

(defface pure-line-active-secondary
  '((t (:inherit mode-line)))
  "Modeline face for active secondary element."
  :group 'pure-line-active)

(defface pure-line-inactive-secondary
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive secondary element."
  :group 'pure-line-inactive)

(defface pure-line-active-tertiary
  '((t (:inherit mode-line)))
  "Modeline face for active tertiary element."
  :group 'pure-line-active)

(defface pure-line-inactive-tertiary
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive tertiary element."
  :group 'pure-line-inactive)

(defface pure-line-hide-mode-line
  `((t :foreground ,(face-foreground 'shadow)
       :background ,(face-background 'default)
       :underline t))
  "Highlight button face."
  :group 'pure-line-hidden)

;;;;; Status Bar Faces

;; pure-line uses a colored symbol to indicate the status of the buffer

(defface pure-line-active-status-RO
  '((t (:inherit (mode-line)
                 :foreground "yellow"
                 (when pure-line-status-invert :inverse-video t))))
  "Modeline face for active READ-ONLY element."
  :group 'pure-line-active)

(defface pure-line-inactive-status-RO
  '((t (:inherit (mode-line-inactive)
                 :foreground "light gray"
                 (when pure-line-status-invert :inverse-video t))))
  "Modeline face for inactive READ-ONLY element."
  :group 'pure-line-inactive)

(defface pure-line-active-status-RW
  '((t (:inherit (mode-line)
                 :foreground "green"
                 (when pure-line-status-invert :inverse-video t))))
  "Modeline face for active READ-WRITE element."
  :group 'pure-line-active)

(defface pure-line-inactive-status-RW
  '((t (:inherit (mode-line-inactive)
                 :foreground "light gray"
                 (when pure-line-status-invert :inverse-video t))))
  "Modeline face for inactive READ-WRITE element."
  :group 'pure-line-inactive)

(defface pure-line-active-status-MD
  '((t (:inherit (mode-line)
                 :foreground "red"
                 (when pure-line-status-invert :inverse-video t))))
  "Modeline face for active MODIFIED element."
  :group 'pure-line-active)

(defface pure-line-inactive-status-MD
  '((t (:inherit (mode-line-inactive)
                 :foreground "light gray"
                 (when pure-line-status-invert :inverse-video t))))
  "Modeline face for inactive MODIFIED element."
  :group 'pure-line-inactive)

;;;;; Bell Faces

(defface pure-line-visual-bell '((t (:background "red")))
  "Face to use for the mode-line when `pure-line-visual-bell-config' is used."
  :group 'pure-line)

;;;; Setup Functions

;;;;; Visual bell for mode line
(defun pure-line-visual-bell-fn ()
  "Blink the status-line red briefly. Set `ring-bell-function' to this to use it."
  (let ((pure-line--bell-cookie
         (if (eq pure-line-position 'bottom)
             (face-remap-add-relative 'mode-line 'pure-line-visual-bell)
           (face-remap-add-relative 'header-line 'pure-line-visual-bell)))
        (force-mode-line-update t))
    (run-with-timer 0.15 nil
                    (lambda (cookie buf)
                      (with-current-buffer buf
                        (face-remap-remove-relative cookie)
                        (force-mode-line-update t)))
                    pure-line--bell-cookie
                    (current-buffer))))

(defun pure-line-visual-bell-config ()
  "Enable flashing the status-line on error."
  (setq ring-bell-function #'pure-line-visual-bell-fn
        visible-bell t))

;;;;; Abbreviate Major-Mode
;; Source: https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
;; TODO - find better icons
(defcustom pure-line-abbrev-alist
  `((dired-mode . " ")
    (emacs-lisp-mode . "")
    (fundamental-mode . "󰝾")
    (helpful-mode . "󰋗")
    (help-mode . "󰋗")
    (lisp-interaction-mode . "")
    (markdown-mode . " ")
    (magit-mode . "󰊢")
    (nxhtml-mode . "")
    (prog-mode . "󰅱")
    (python-mode . "")
    (text-mode . ""))
  "Alist for `pure-line--abbrev'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *as substitute for* the original."
  :type '(alist
          :key-type (symbol :tag "Major mode")
          :value-type (string :tag "Abbreviation"))
  :group 'pure-line)

(defun pure-line--abbrev ()
  (cl-loop for abbrev in pure-line-abbrev-alist
           do (let* ((mode (car abbrev))
                     (mode-str (cdr abbrev))
                     (old-mode-str (cdr (assq mode minor-mode-alist))))
                (when old-mode-str
                  (setcar old-mode-str mode-str))
                ;; major mode
                (when (eq mode major-mode)
                  (setq mode-name mode-str)))))

;; Set abbrev (default is nil)
(when pure-line-abbrev
  (add-hook 'after-change-major-mode-hook #'pure-line--abbrev))

;;;;; Mode Name
(defun pure-line-user-mode-p ()
  "Should the user supplied mode be called for modeline?"
  pure-line-user-mode)

(defun pure-line-mode-name ()
  "Return current major mode name."
  (format-mode-line mode-name))

;;;;; String Truncate
(defun pure-line-truncate (str size &optional ellipsis)
  "If STR is longer than SIZE, truncate it and add ELLIPSIS."

  (let ((ellipsis (or ellipsis "…")))
    (if (> (length str) size)
        (format "%s%s" (substring str 0 (- size (length ellipsis))) ellipsis)
      str)))

;;;;; Branch display
;; -------------------------------------------------------------------
(defun pure-line-project-name ()
  "Return name of project without path."
  (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-"))))

(defun pure-line-vc-project-branch ()
  "Show project and branch name for file.
Otherwise show '-'."
  (let ((backend (vc-backend buffer-file-name)))
    (concat
     (if buffer-file-name
         (if vc-mode
             (let ((project-name (pure-line-project-name)))
               ;; Project name
               (unless (string= "-" project-name)
                 (concat
                  ;; Divider
                  (propertize " •" 'face `(:inherit fringe))
                  (format " %s" project-name))))))

     ;; Show branch
     (if vc-mode
         (concat
          pure-line-vc-symbol
          (substring-no-properties
           vc-mode
           (+ (if (eq backend 'Hg) 2 3) 2)))  nil))))

;;;;; Dir display
;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun pure-line-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;;;;; Git diff in modeline
;; https://cocktailmake.github.io/posts/emacs-modeline-enhancement-for-git-diff/
(when pure-line-git-diff-mode-line
  (defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
    "Show the information of git diff in status-line"
    (setq ad-return-value
          (concat ad-return-value
                  (let ((plus-minus (vc-git--run-command-string
                                     file "diff" "--numstat" "--")))
                    (if (and plus-minus
                             (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
                        (concat
                         " "
                         (format "+%s" (match-string 1 plus-minus))
                         (format "-%s" (match-string 2 plus-minus)))
                      ""))))))

;;;;; Git Parse Repo Status
;; See https://kitchingroup.cheme.cmu.edu/blog/2014/09/19/A-git-status-Emacs-modeline/
(defun pure-line-git-parse-status ()
  "Display the status of the repo."
  (interactive)
  (let ((U 0)   ; untracked files
        (M 0)   ; modified files
        (O 0)   ; other files
        (U-files "")
        (M-files "")
        (O-files ""))
    (dolist (line (split-string
                   (shell-command-to-string "git status --porcelain")
                   "\n"))
      (cond

       ;; ignore empty line at end
       ((string= "" line) nil)

       ((string-match "^\\?\\?" line)
        (setq U (+ 1 U))
        (setq U-files (concat U-files "\n" line)))

       ((string-match "^ M" line)
        (setq M (+ 1 M))
        (setq M-files (concat M-files "\n" line))
        )))

    ;; construct propertized string
    (concat
     (propertize
      (format "M:%d" M)
      'face (if (> M 0)
                'error
              'success)
      'help-echo M-files)
     (propertize "|" 'face 'magit-dimmed)
     (propertize
      (format "?:%d" U)
      'face (if (> U 0)
                'warning
              'success)
      'help-echo U-files))))

;;;;; Flycheck/Flymake Segment
(defvar-local pure-line--flycheck-text nil)

(customize-set-variable 'flymake-mode-line-title " ")

(defun pure-line--update-flycheck-segment (&optional status)
  "Update `pure-line--flycheck-text' against the reported flycheck STATUS."
  (setq pure-line--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat pure-line-flycheck-label
                                                 (number-to-string sum)
                                                 " ")
                                         'face (if .error
                                                   'error
                                                 'warning))))
                       (propertize "Good " 'face 'success)))
          ('running (propertize "Checking " 'face 'info))
          ('errored (propertize "Error " 'face 'error))
          ('interrupted (propertize "Paused " 'face 'fringe))
          ('no-checker ""))))

(defun pure-line-check-syntax ()
  "Display syntax-checking information from flymake/flycheck
in the mode-line (if available)."
  (if (and (>= emacs-major-version 28)
           (boundp 'flymake-mode)
           flymake-mode)
      (concat (format-mode-line flymake-mode-line-format) " ")
    pure-line--flycheck-text))

(defun pure-line-show-func ()
  "Display `which-function-mode' output in mode-line."
  (if (and (boundp 'which-function-mode)
           (default-value 'which-function-mode))
      (concat (format-mode-line which-func-format) " ")
    ""))

;;;;; Display-time-mode

(defun pure-line-time ()
  "Date using given FORMAT and DATE"

  (if display-time-day-and-date
      (propertize (format-time-string pure-line-time-day-and-date-format))
    (propertize (format-time-string pure-line-time-format ) 'face `(:height 0.9))))

;;;;; Status
(defun pure-line-status ()
  "Return buffer status, one of 'read-only, 'modified or 'read-write."

  (let ((read-only  (when (not (or (derived-mode-p 'vterm-mode)
                                   (derived-mode-p 'term-mode)
                                   (derived-mode-p 'Info-mode)
                                   (derived-mode-p 'help-mode)
                                   (derived-mode-p 'helpful-mode)
                                   (derived-mode-p 'elfeed-search)
                                   (derived-mode-p 'elfeed-show)))
                      buffer-read-only))
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  'modified)
          (read-only 'read-only)
          (t         'read-write))))

;;;;; Compose Status-Line
(defun pure-line-compose (status name primary tertiary secondary)
  "Compose a string with provided information.
Each section is first defined, along with a measure of the width of the status-line.
STATUS, NAME, PRIMARY, and SECONDARY are always displayed. TERTIARY is displayed only in some modes."
  (let* ((window (get-buffer-window (current-buffer)))

         (name-max-width (max 12
                              (- (window-body-width)
                                 (round (* 0.8 (length primary)))
                                 (length tertiary)
                                 (length secondary))))

         (name (if (and (stringp name) (> (length name) name-max-width))
                   (format "…%s" (substring name (- (length name) name-max-width -1)))
                 name))

         (status (or status (pure-line-status)))

         (active (eq window pure-line--selected-window))

         (prefix (cond ((eq pure-line-prefix nil) "")
                       (t
                        (cond ((derived-mode-p 'term-mode) " ")
                              ((derived-mode-p 'vterm-mode) " ")
                              ((derived-mode-p 'eshell-mode) " ")
                              ((derived-mode-p 'Info-mode) " ")
                              ((derived-mode-p 'help-mode) " 󰋗")
                              ((derived-mode-p 'helpful-mode) " 󰋗")
                              ((eq status 'read-only)
                               (if (display-graphic-p) pure-line-gui-ro-symbol
                                 pure-line-tty-ro-symbol))
                              ((eq status 'read-write) (if (display-graphic-p) pure-line-gui-rw-symbol
                                                         pure-line-tty-rw-symbol))
                              ((eq status 'modified)   (if (display-graphic-p) pure-line-gui-mod-symbol
                                                         pure-line-tty-mod-symbol))
                              ((window-dedicated-p) (if (display-graphic-p) " ––" " --"))
                              ;; otherwise just use rw symbol
                              (t (if (display-graphic-p) pure-line-gui-rw-symbol
                                   pure-line-tty-rw-symbol))))))

         (face-modeline (if active
                            'pure-line-active
                          'pure-line-inactive))

         (face-prefix (if (not prefix) face-modeline
                        (if active
                            (cond ((eq status 'read-only)  'pure-line-active-status-RO)
                                  ((eq status 'read-write) 'pure-line-active-status-RW)
                                  ((eq status 'modified)   'pure-line-active-status-MD)
                                  ((or (derived-mode-p 'term-mode)
                                       (derived-mode-p 'vterm-mode)
                                       (derived-mode-p 'eshell-mode))
                                   'pure-line-active-status-MD)
                                  ((or (derived-mode-p 'Info-mode)
                                       (derived-mode-p 'help-mode)
                                       (derived-mode-p 'helpful-mode))
                                   'pure-line-active-status-RO)
                                  (t                       'pure-line-active))
                          (cond ((eq status 'read-only)  'pure-line-inactive-status-RO)
                                ((eq status 'read-write) 'pure-line-inactive-status-RW)
                                ((eq status 'modified)   'pure-line-inactive-status-MD)
                                ((or (derived-mode-p 'term-mode)
                                     (derived-mode-p 'vterm-mode)
                                     (derived-mode-p 'eshell-mode)
                                     (derived-mode-p 'Info-mode)
                                     (derived-mode-p 'help-mode)
                                     (derived-mode-p 'helpful-mode))
                                 'pure-line-inactive-status-RW)
                                (t                       'pure-line-inactive)))))
         (face-name (if active
                        'pure-line-active-name
                      'pure-line-inactive-name))
         (face-primary (if active
                           'pure-line-active-primary
                         'pure-line-inactive-primary))
         (face-secondary (if active
                             'pure-line-active-secondary
                           'pure-line-inactive-secondary))
         (face-tertiary (if active
                            'pure-line-active-tertiary
                          'pure-line-inactive-tertiary))
         (left
          ;; special face for special mode prefixes
          (concat
           (propertize prefix 'face face-prefix 'display `(raise ,pure-line-symbol-position))
           ;; this matters for inverse-video!
           (propertize " " 'face face-prefix  'display `(raise ,pure-line-space-top))

           (when pure-line-prefix-padding
             (propertize " " 'face face-modeline))

           (propertize name 'face face-name)

           (propertize " "  'face (if active 'pure-line-active
                                    'pure-line-inactive)
                       'display `(raise ,pure-line-space-bottom))

           (propertize primary 'face face-primary)))

         (right (concat tertiary (propertize secondary 'face face-secondary) (propertize  pure-line-hspace 'face face-modeline)))

         (right-len (length (format-mode-line right))))
    (concat
     left
     (propertize " " 'face face-modeline 'display `(space :align-to (- right ,right-len)))
     right)))

;;;; Mode Functions
;;;; Default display
(defun pure-line-default-mode ()
  "Compose the default status line."
  (let ((buffer-name (format-mode-line (if buffer-file-name
                                           (file-name-nondirectory (buffer-file-name))
                                         "%b")))
        (mode-name   (pure-line-mode-name))
        (branch      (pure-line-vc-project-branch))
        (position    (format-mode-line "%l:%c:%o")))
    (pure-line-compose (pure-line-status)
                       (pure-line-truncate buffer-name pure-line-truncate-value)
                       (concat pure-line-display-group-start
                               mode-name
                               (when branch
                                 branch)
                               pure-line-display-group-end)
                       ""
                       ;; Narrowed buffer
                       (concat (if (buffer-narrowed-p)
                                   (concat
                                    (propertize "⇥ "  'face `(:inherit pure-line-inactive-secondary))
                                    position " ")
                                 position)
                               (pure-line-time)))))

;;;;; Prog Mode
;; ---------------------------------------------------------------------
(defun pure-line-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun pure-line-prog-mode ()
  (let ((buffer-name (format-mode-line (if buffer-file-name (file-name-nondirectory (buffer-file-name)) "%b")))
        (mode-name   (pure-line-mode-name))
        (branch      (pure-line-vc-project-branch))
        (position    (format-mode-line "%l:%c:%o")))
    (pure-line-compose (pure-line-status)
                       (pure-line-truncate buffer-name pure-line-truncate-value)
                       (concat pure-line-display-group-start mode-name
                               (when branch branch)
                               pure-line-display-group-end)

                       (concat
                        (if pure-line-which-func
                            (pure-line-show-func) "")
                        (if pure-line-syntax
                            (pure-line-check-syntax) ""))

                       (concat
                        ;; Narrowed buffer
                        (when (buffer-narrowed-p)
                          (propertize "⇥ "  'face `(:inherit pure-line-inactive-secondary)))
                        (if pure-line-syntax
                            (if (or (boundp 'flycheck-mode)
                                    (boundp 'flymake-mode))
                                ;; (concat position pure-line-hspace)
                                position)
                          position)

                        (pure-line-time)))))

(defun pure-line-prog-activate ()
  "Setup flycheck hooks."
  (add-hook 'flycheck-status-changed-functions #'pure-line--update-flycheck-segment)
  (add-hook 'flycheck-mode-hook #'pure-line--update-flycheck-segment)
  (when pure-line-git-diff-mode-line
    (add-hook 'after-save-hook #'vc-refresh-state)))

(defun pure-line-prog-deactivate ()
  "Remove flycheck hooks."
  (remove-hook 'flycheck-status-changed-functions #'pure-line--update-flycheck-segment)
  (remove-hook 'flycheck-mode-hook #'pure-line--update-flycheck-segment)
  (when pure-line-git-diff-mode-line
    (remove-hook 'after-save-hook #'vc-refresh-state)))

;;;;; Fundamental Mode

(defun pure-line-fundamental-mode-p ()
  (derived-mode-p 'fundamental-mode))

(defun pure-line-fundamental-mode ()
  (pure-line-default-mode))

;;;;; Text Mode

(defun pure-line-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun pure-line-text-mode ()
  (pure-line-default-mode))


;;;;; Help (& Helpful) Mode
(defun pure-line-help-mode-p ()
  (derived-mode-p 'help-mode))

(defun pure-line-helpful-mode-p ()
  (derived-mode-p 'helpful-mode))

(defun pure-line-help-mode ()
  (pure-line-compose "HELP"
                     (format-mode-line "%b")
                     ""
                     ""
                     (format-mode-line "%l:%c:%o")))


;;;;; Info Display
;; ---------------------------------------------------------------------
(defun pure-line-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
        (node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
        line)
    (save-excursion
      (while  (> depth 0)
        (setq node (nth 1 (assoc node nodes)))
        (if node (push node crumbs))
        (setq depth (1- depth)))
      (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
                                   crumbs (cons nil crumbs))))
      (forward-line 1)
      (dolist (node crumbs)
        (let ((text
               (if (not (equal node "Top")) node
                 (format "%s"
                         (if (stringp Info-current-file)
                             (file-name-sans-extension
                              (file-name-nondirectory Info-current-file))
                           Info-current-file)))))
          (setq line (concat line (if (null line) "" " > ")
                             (if (null node) "..." text)))))
      (if (and cnode (not (equal cnode "Top")))
          (setq line (concat line (if (null line) "" " > ") cnode)))
      line)))

(defun pure-line-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun pure-line-info-mode ()
  (pure-line-compose "INFO"
                     ""
                     (concat pure-line-display-group-start
                             (pure-line-info-breadcrumbs)
                             pure-line-display-group-end)
                     ""
                     ""
                     ))

(defun pure-line-info-activate ()
  (if (eq pure-line-position 'top)
      (setq Info-use-header-line nil)))

(defun pure-line-info-deactivate ()
  (custom-reevaluate-setting 'Info-use-header-line))

;;;; Term & Vterm
;; ---------------------------------------------------------------------
;; term
(defun pure-line-term-mode-p ()
  (derived-mode-p 'term-mode))

;; vterm
(defun pure-line-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun pure-line-term-mode ()
  (pure-line-compose " >_ "
                     "Terminal"
                     (concat pure-line-display-group-start
                             (file-name-nondirectory shell-file-name)
                             pure-line-display-group-end)
                     nil
                     (concat (pure-line-shorten-directory default-directory 32)
                             (pure-line-time))))


;; ---------------------------------------------------------------------

(defun pure-line-get-ssh-host (_str)
  (let ((split-defdir (split-string default-directory)))
    (if (equal (length split-defdir) 1)
        (car (split-string (shell-command-to-string "hostname") "\n"))
      (cadr split-defdir))))

(defun pure-line-ssh-mode ()
  (pure-line-compose " >_ "
                     "Terminal"
                     (concat pure-line-display-group-start
                             (pure-line-get-ssh-host default-directory)
                             pure-line-display-group-end)
                     nil
                     (concat (pure-line-shorten-directory (car (last (split-string default-directory ":"))) 32)
                             (pure-line-time))))

;;;; Eshell
;; ---------------------------------------------------------------------
(defun pure-line-eshell-mode-p ()
  (derived-mode-p 'eshell-mode))

(defun pure-line-eshell-mode ()
  (pure-line-compose " >_ "
                     "Eshell"
                     (concat pure-line-display-group-start
                             (buffer-name)
                             pure-line-display-group-end)
                     ""
                     (concat (pure-line-shorten-directory default-directory 32)
                             (pure-line-time))))

(defun pure-line-esh-activate ()
  (with-eval-after-load 'esh-mode
    (setq eshell-status-in-mode-line nil)))

(defun pure-line-esh-deactivate ()
  (custom-reevaluate-setting 'eshell-status-in-mode-line))

;;;; Messages Buffer Mode
;; ---------------------------------------------------------------------
(defun pure-line-messages-mode-p ()
  (derived-mode-p 'messages-buffer-mode))

(defun pure-line-messages-mode ()
  (pure-line-compose (pure-line-status)
                     "*Messages*"
                     ""
                     ""
                     (concat "" (pure-line-time))))

;;;; Message Mode
;; ---------------------------------------------------------------------
(defun pure-line-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun pure-line-message-mode ()
  (pure-line-compose (pure-line-status)
                     "Message" "(Draft)" nil (pure-line-time)))

;;;; Docview Mode
;;---------------------------------------------------------------------
(defun pure-line-doc-view-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun pure-line-doc-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (pure-line-mode-name))
        (branch      (pure-line-vc-project-branch))
        (page-number (concat
                      (number-to-string (doc-view-current-page)) "/"
                      (or (ignore-errors
                            (number-to-string (doc-view-last-page-number)))
                          "???"))))
    (pure-line-compose
     (pure-line-status)
     buffer-name
     (concat pure-line-display-group-start mode-name
             branch
             pure-line-display-group-end)
     nil
     (concat page-number
             (pure-line-time)))))

;;;; PDF View Mode
;; ---------------------------------------------------------------------
(defun pure-line-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(with-eval-after-load 'pdf-tools
  (require 'pdf-view))

(defun pure-line-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (pure-line-mode-name))
        (branch      (pure-line-vc-project-branch))
        (page-number (concat
                      (number-to-string (eval `(pdf-view-current-page))) "/"
                      (or (ignore-errors
                            (number-to-string (pdf-cache-number-of-pages)))
                          "???"))))
    (pure-line-compose (pure-line-status)
                       buffer-name
                       (concat pure-line-display-group-start mode-name
                               pure-line-display-group-end)
                       nil
                       (concat page-number " " (pure-line-time)))))

;;;; MenuMode

(defun pure-line-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun pure-line-buffer-menu-mode ()
  (let ((buffer-name "Buffer list")
        (mode-name   (pure-line-mode-name))
        (position    (format-mode-line "%l:%c:%o")))

    (pure-line-compose (pure-line-status)
                       buffer-name "" nil (concat position pure-line-hspace (pure-line-time)))))

;;;; Imenu-List
(defun pure-line-imenu-list-mode-p ()
  (derived-mode-p 'imenu-list-major-mode))

(defun pure-line-imenu-list-mode ()
  (let (
        ;; We take into account the case of narrowed buffers
        (buffer-name (buffer-name imenu-list--displayed-buffer))
        (branch      (pure-line-vc-project-branch))
        (position    (format-mode-line "%l:%c")))
    (pure-line-compose (pure-line-status)
                       buffer-name
                       "(imenu list)"
                       ""
                       "")))
;;;; Completion
;; ---------------------------------------------------------------------
(defun pure-line-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun pure-line-completion-list-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (pure-line-mode-name))
        (position    (format-mode-line "%l:%c:%o")))

    (pure-line-compose (pure-line-status)
                       buffer-name "" nil (concat position pure-line-hspace))))

;;;; Deft Mode

(with-eval-after-load 'deft
  (defun pure-line--deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun pure-line-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun pure-line-deft-mode ()
  (let ((prefix " DEFT ")
        (primary "Search:")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (pure-line-compose prefix primary filter nil matches)))

;;;; Calendar Mode
;; ---------------------------------------------------------------------
(defun pure-line-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun pure-line-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun pure-line-calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                              :height 0.5
                              :background ,(face-background 'default)))))

(defun pure-line-calendar-activate ()
  (with-eval-after-load 'calendar
    (add-hook 'calendar-initial-window-hook
              #'pure-line-calendar-setup-header)))

(defun pure-line-calendar-deactivate ()
  (remove-hook 'calendar-initial-window-hook
               #'pure-line-calendar-setup-header))

;;;; Org Capture
;; ---------------------------------------------------------------------
(defun pure-line-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun pure-line-org-capture-mode ()
  (pure-line-compose (pure-line-status)
                     "Capture"
                     (concat pure-line-display-group-start
                             (org-capture-get :description)
                             pure-line-display-group-end)
                     nil
                     "Finish: C-c C-c, refile: C-c C-w, cancel: C-c C-k "))


(defun pure-line-org-capture-turn-off-header-line ()
  (setq-local header-line-format (default-value 'header-line-format))
  (message nil))

(defun pure-line-org-capture-activate ()
  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook
              #'pure-line-org-capture-turn-off-header-line)))

(defun pure-line-org-capture-deactivate ()
  (remove-hook 'org-capture-mode-hook
               #'pure-line-org-capture-turn-off-header-line))


;;;; Org Agenda
;; ---------------------------------------------------------------------
(defun pure-line-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun pure-line-org-agenda-mode ()
  (let ((pure-line-icon-time t))
    (pure-line-compose (pure-line-status)
                       "Agenda"
                       (concat pure-line-display-group-start (format "%S" org-agenda-current-span) pure-line-display-group-end)
                       ""
                       (concat (format-time-string "%A, %d %B %Y")
                               (pure-line-time)
                               (format-time-string " %H:%M")))))

;;;; Org Clock
;; ---------------------------------------------------------------------
(defun pure-line-org-clock-mode-p ()
  (and (boundp 'org-mode-line-string)
       (stringp org-mode-line-string)))

(defun pure-line-org-clock-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (pure-line-mode-name))
        (branch      (pure-line-vc-project-branch))
        (position    (format-mode-line "%l:%c:%o")))
    (pure-line-compose (pure-line-status)
                       buffer-name
                       (concat pure-line-display-group-start
                               mode-name
                               (when branch
                                 branch)
                               pure-line-display-group-end)
                       ""
                       (concat
                        ;; Narrowed buffer
                        (when (buffer-narrowed-p)
                          (propertize "⇥ "  'face `(:inherit pure-line-inactive-secondary)))
                        org-mode-line-string
                        " "
                        nil
                        position
                        pure-line-hspace))))

(defun pure-line-org-clock-out ()
  (setq org-mode-line-string nil)
  (force-mode-line-update))

(defun pure-line-org-clock-activate ()
  (with-eval-after-load 'org-clock
    (add-hook 'org-clock-out-hook #'pure-line-org-clock-out)))

(defun pure-line-org-clock-deactivate ()
  (remove-hook 'org-clock-out-hook
               #'pure-line-org-clock-out))

;;;; Elfeed
;; ---------------------------------------------------------------------
(defun pure-line-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun pure-line-elfeed-search-mode ()
  (let* ((status  "NEWS")
         (no-database (zerop (elfeed-db-last-update)))
         (update      (> (elfeed-queue-count-total) 0))

         (name  (cond (no-database "No database")
                      (update      "Update:")
                      (t           "Search:")))
         (primary (cond  (no-database "")
                         (update
                          (let ((total (elfeed-queue-count-total))
                                (in-process (elfeed-queue-count-active)))
                            (format "%d jobs pending, %d active"
                                    (- total in-process) in-process)))
                         (t  (let* ((db-time (seconds-to-time (elfeed-db-last-update)))
                                    (unread))
                               (cond (elfeed-search-filter-active "")
                                     ((string-match-p "[^ ]" elfeed-search-filter)
                                      elfeed-search-filter)
                                     (""))))))
         (secondary (concat
                     (cond
                      ((zerop (elfeed-db-last-update)) "")
                      ((> (elfeed-queue-count-total) 0) "")
                      (t (elfeed-search--count-unread)))
                     (pure-line-time))))

    (pure-line-compose status name primary nil secondary)))

;; Elfeed uses header-line, we need to tell it to use our own format
(defun pure-line-elfeed-setup-header ()
  (setq header-line-format (default-value 'header-line-format)))

(defun pure-line-elfeed-search-activate ()
  (with-eval-after-load 'elfeed
    (if (eq pure-line-position 'top)
        (setq elfeed-search-header-function #'pure-line-elfeed-setup-header))))

(defun pure-line-elfeed-search-deactivate ()
  (if (boundp 'elfeed-search-header-function)
      (setq elfeed-search-header-function #'elfeed-search--header)))

;; ---------------------------------------------------------------------
(defun pure-line-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun pure-line-elfeed-show-mode ()
  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tags-str (mapconcat #'symbol-name tags ", "))
         (tag          (if tags
                           (concat pure-line-display-group-start
                                   tags-str
                                   pure-line-display-group-end)
                         " "))
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (entry-author (elfeed-meta elfeed-show-entry :author))
         (feed-title (if entry-author
                         (concat entry-author " (" (elfeed-feed-title feed) ")")
                       (elfeed-feed-title feed))))
    (pure-line-compose
     ""
     (pure-line-truncate title 65)
     tag
     ""
     (format-time-string "%Y-%m-%d %H:%M:%S" date))))


;;;; Mu4e

(defun pure-line-mu4e-last-query ()
  "Get the most recent mu4e query or nil if there is none."
  (if (fboundp 'mu4e-last-query)
      (mu4e-last-query)
    mu4e~headers-last-query))

(defun pure-line-mu4e-context ()
  "Return the current mu4e context as a non propertized string."
  (if (> (length (mu4e-context-name (mu4e-context-current))) 0)
      (concat
       pure-line-display-group-start
       (substring-no-properties (mu4e-context-name (mu4e-context-current)))
       pure-line-display-group-end)
    "(none)"))

(defun pure-line-mu4e-server-props ()
  "Encapsulates the call to the variable mu4e-/~server-props
depending on the version of mu4e."
  (if (version< mu4e-mu-version "1.6.0")
      mu4e~server-props
    mu4e--server-props))

(defun pure-line-mu4e-activate ()
  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'pure-line)))

(defun pure-line-mu4e-deactivate ()
  (advice-remove #'mu4e~header-line-format #'pure-line))

;; ---------------------------------------------------------------------
(defun pure-line-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun pure-line-mu4e-dashboard-mode ()
  (pure-line-compose (pure-line-status)
                     (format "%d messages"
                             (plist-get (pure-line-mu4e-server-props) :doccount))
                     ""
                     ""
                     (pure-line-time)))

;; ---------------------------------------------------------------------
(defun pure-line-mu4e-loading-mode-p ()
  (derived-mode-p 'mu4e-loading-mode))

(defun pure-line-mu4e-loading-mode ()
  (pure-line-compose (pure-line-status)
                     (format-time-string "%A %d %B %Y, %H:%M ")
                     ""
                     "Loading..."
                     (pure-line-mu4e-context)))

;; ---------------------------------------------------------------------
(defun pure-line-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun pure-line-mu4e-main-mode ()
  (pure-line-compose (pure-line-status)
                     (format-time-string "%A %d %B %Y, %H:%M ")
                     ""
                     ""
                     (pure-line-mu4e-context)))

;; ---------------------------------------------------------------------
(defun pure-line-mu4e-compose-mode-p ()
  (derived-mode-p 'mu4e-compose-mode))

(defun pure-line-mu4e-compose-mode ()
  (pure-line-compose (pure-line-status)
                     (format-mode-line "%b")
                     ""
                     ""
                     (format "[%s] "
                             (pure-line-mu4e-quote
                              (mu4e-context-name (mu4e-context-current))))))

;; ---------------------------------------------------------------------
(defun pure-line-mu4e-quote (str)
  (if (version< mu4e-mu-version "1.8.0")
      (mu4e~quote-for-modeline str)
    (mu4e-quote-for-modeline str)))

(defun pure-line-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun pure-line-mu4e-headers-mode ()
  (let ((mu4e-modeline-max-width 80))
    (pure-line-compose
     (pure-line-status)
     "Search:"
     (or (pure-line-mu4e-quote
          (pure-line-mu4e-last-query)) "")
     ""
     (concat
      (format "[%s] "
              (pure-line-mu4e-quote
               (mu4e-context-name (mu4e-context-current))))
      (or (pure-line-time) "")))))

;; ---------------------------------------------------------------------
(defun pure-line-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun pure-line-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date    (mu4e-message-field msg :date)))
    (pure-line-compose (pure-line-status)
                       (or from "")
                       (concat pure-line-display-group-start
                               (pure-line-truncate (or subject "") 50 "…")
                               pure-line-display-group-end)
                       ""
                       (concat (or (format-time-string mu4e-headers-date-format date) "") " "))))

(defun pure-line-mu4e-activate ()
  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'pure-line)))

(defun pure-line-mu4e-deactivate ()
  (advice-remove #'mu4e~header-line-format #'pure-line))

;;;; Ein

(defun pure-line-ein-notebook-mode ()
  (let ((buffer-name (format-mode-line "%b")))
    (pure-line-compose (if (ein:notebook-modified-p) "MD" "RW")
                       buffer-name
                       ""
                       ""
                       (concat
                        (ein:header-line)
                        (pure-line-time)))))

;; since the EIN library itself is constantly re-rendering the notebook, and thus
;; re-setting the header-line-format, we cannot use the pure-line function to set
;; the header format in a notebook buffer. Fortunately, EIN exposes the
;; ein:header-line-format variable for just this purpose.

(defun pure-line-ein-notebook-activate ()
  (with-eval-after-load 'ein
    (if (eq pure-line-position 'top)
        (setq ein:header-line-format '((:eval (pure-line-ein-notebook-mode)))))))

(defun pure-line-ein-notebook-deactivate ()
  (if (boundp 'ein:header-line-format)
      (setq ein:header-line-format '(:eval (ein:header-line)))))


;;;; Buffer Menu Mode
;; ---------------------------------------------------------------------
(defun pure-line-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun pure-line-buffer-menu-mode ()
  (let ((buffer-name "Buffer list")
        (mode-name   (pure-line-mode-name))
        (position    (format-mode-line "%l:%c")))

    (pure-line-compose nil
                       buffer-name
                       ""
                       nil
                       (concat
                        position
                        (pure-line-time)))))

;;(defun buffer-menu-mode-header-line ()
;;  (face-remap-add-relative
;;   'header-line `(:background ,(face-background 'nano-subtle))))
;;(add-hook 'Buffer-menu-mode-hook
;;          #'buffer-menu-mode-header-line)

(defun pure-line-buffer-menu-activate ()
  (if (eq pure-line-position 'top)
      (setq Buffer-menu-use-header-line nil)))

(defun pure-line-buffer-menu-deactivate ()
  (custom-reevaluate-setting 'Buffer-menu-use-header-line))

;;;; Elpher Mode
;; ---------------------------------------------------------------------
(defun pure-line-elpher-mode-p ()
  (derived-mode-p 'elpher-mode))

(defun pure-line-elpher-mode ()
  (let* ((display-string (elpher-page-display-string elpher-current-page))
         (sanitized-display-string (replace-regexp-in-string "%" "%%" display-string))
         (address (elpher-page-address elpher-current-page))
         (tls-string (if (and (not (elpher-address-about-p address))
                              (member (elpher-address-protocol address)
                                      '("gophers" "gemini")))
                         "(TLS encryption)"
                       "")))
    (pure-line-compose nil
                       sanitized-display-string
                       tls-string
                       nil
                       (pure-line-time))))

(defun pure-line-elpher-activate ()
  (with-eval-after-load 'elpher
    (setq elpher-use-header nil)))

;;;; Ispell Mode
;; ---------------------------------------------------------------------
(defun pure-line-enlarge-ispell-choices-buffer (buffer)
  (when (string= (buffer-name buffer) "*Choices*")
    (with-current-buffer buffer
      ;; (enlarge-window +2)
      (setq-local header-line-format nil)
      (setq-local mode-line-format nil))))

(defun pure-line-ispell-activate ()
  (with-eval-after-load 'ispell
    (advice-add #'ispell-display-buffer :after
                #'pure-line-enlarge-ispell-choices-buffer)))

(defun pure-line-ispell-deactivate ()
  (advice-remove #'ispell-display-buffer
                 #'pure-line-enlarge-ispell-choices-buffer))

;;;; Eldoc
;; ---------------------------------------------------------------------
;; `eldoc-minibuffer-message' changes `mode-line-format' but status-line when
;; `pure-line-position' is `top' fails to display info. Solution is to move
;; eldoc messages to the minibuffer/echo area.
(when (eq pure-line-position 'top)
  (setq eldoc-message-function #'message))

;;;; Magit
;; ---------------------------------------------------------------------
(defun pure-line-magit-mode-p ()
  (derived-mode-p 'magit-mode))

;; Add functions to parse repo every N seconds
(defvar pure-line-git-parse-last-update (float-time) "Last time we updated")
(defvar pure-line-git-parse-update-interval 15 "Minimum time between update in seconds")
(defvar pure-line-git-parse "" "Last value of the parse")

(defun pure-line-magit-mode ()
  (let* ((buffer-name (format-mode-line
                       (if buffer-file-name
                           (file-name-nondirectory (buffer-file-name))
                         "%b")))
         (mode-name   (pure-line-mode-name))
         (project     (file-name-nondirectory (directory-file-name (magit-toplevel))))
         (branch      (magit-get-current-branch))
         (status      (pure-line-git-parse-status)))
    (pure-line-compose (pure-line-status)
                       mode-name
                       (concat pure-line-display-group-start
                               project
                               pure-line-vc-symbol
                               branch
                               pure-line-display-group-end)
                       status
                       "")))

;;;; Setup Pure-line
;; ---------------------------------------------------------------------
(defun pure-line-face-clear (face)
  "Clear FACE"
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    'unspecified))

;; ---------------------------------------------------------------------
(defvar pure-line--saved-mode-line-format nil)
(defvar pure-line--saved-header-line-format nil)
(defvar pure-line--selected-window nil)

(defun pure-line--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq pure-line--selected-window (selected-window)))

(defun pure-line ()
  "Build and set the modeline."
  (let* ((format
          '((:eval
             (funcall
              (or (catch 'found
                    (dolist (elt pure-line-mode-formats)
                      (let* ((config (cdr elt))
                             (mode-p (plist-get config :mode-p))
                             (format (plist-get config :format)))
                        (when mode-p
                          (when (funcall mode-p)
                            (throw 'found format))))))
                  pure-line-default-mode-format))))))
    (if (eq pure-line-position 'top)
        (progn
          (setq header-line-format format)
          (setq-default header-line-format format))
      (progn
        (setq mode-line-format format)
        (setq-default mode-line-format format)))))

(defun pure-line-update-windows ()
  "Hide the mode line depending on the presence of a window
below or a buffer local variable 'no-mode-line'."
  (dolist (window (window-list))
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        (if (or (not (boundp 'no-mode-line)) (not no-mode-line))
            (setq mode-line-format
                  (cond ((one-window-p t) (list ""))
                        ((eq (window-in-direction 'below) (minibuffer-window)) (list ""))
                        ((not (window-in-direction 'below)) (list ""))
                        (t nil))))))))

(defun pure-line-mode--activate ()
  "Activate pure-line."

  ;; Save current mode-line and header-line
  (unless pure-line--saved-mode-line-format
    (setq pure-line--saved-mode-line-format mode-line-format)
    (setq pure-line--saved-header-line-format header-line-format))

  (dolist (elt pure-line-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-activate)))
      (when fn (funcall fn))))

  (run-hooks 'pure-line-mode-format-activate-hook)

  ;; Update selected window
  (pure-line--update-selected-window)
  ;; (setq pure-line--selected-window (selected-window))

  (setq         mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq         header-line-format nil)
  (setq-default header-line-format nil)

  (pure-line)

  ;; Use pure-line-visual-bell when var is set to t
  (when pure-line-visual-bell
    (pure-line-visual-bell-config))

  ;; This hooks is necessary to register selected window because when
  ;;  a modeline is evaluated, the corresponding window is always selected.
  (add-hook 'post-command-hook #'pure-line--update-selected-window)

  ;; This hooks hide the modeline for windows having a window below them
  ;; Disabled for the time being,
  ;;  -> see https://github.com/rougier/nano-modeline/issues/24
  ;; (add-hook 'window-configuration-change-hook #'pure-line-update-windows)

  (force-mode-line-update t))

;; Deactivate status-line
(defun pure-line-mode--deactivate ()
  "Deactivate pure-line and restore default mode-line."

  (dolist (elt pure-line-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-deactivate)))
      (when fn (funcall fn))))

  (run-hooks 'pure-line-mode-format-deactivate-hook)

  (remove-hook 'post-command-hook
               #'pure-line--update-selected-window)
  (remove-hook 'window-configuration-change-hook
               #'pure-line-update-windows)

  ;; Deactivate pure-line-visual-bell
  (setq pure-line-visual-bell nil)

  (setq         mode-line-format pure-line--saved-mode-line-format)
  (setq-default mode-line-format pure-line--saved-mode-line-format)
  (setq         header-line-format pure-line--saved-header-line-format)
  (setq-default header-line-format pure-line--saved-header-line-format))



;;;; Pure-line minor mode

;; Store the default mode-line format
(defvar pure-line--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode pure-line-mode
  "Toggle pure-line on or off."
  :group 'pure-line
  :global t
  :lighter nil

  (if pure-line-mode
      (pure-line-mode--activate)
    (pure-line-mode--deactivate))

  ;; Run any registered hooks
  (run-hooks 'pure-line-mode-hook))

;;; Provide:
(provide 'pure-line)

;;; pure-line.el ends here
