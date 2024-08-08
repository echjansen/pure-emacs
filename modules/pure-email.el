;;; pure-email.el --- Pure Emacs's Email. -*- lexical-binding: t -*-

;; Copyright (C) 2024 ECHJansen

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

;; Email on Emacs is almost like a separate application, and so it
;; is provided as a stand alone feature to be added if required.

;; This configuration is based on the mu4e extension to mu (mail utility).
;; It needs to be installed separate from Emacs, something like:
;; yay -S mu isync msmtp
;; or
;; sudo apt-get install mu isync msmtp

;; Resource: https://www.djcbsoftware.nl/code/mu/mu4e/index.html

;; ===========================================================================
;; To receive email (synchronizing to the mail server)
;; ===========================================================================
;; ;; Sample configuration for ~/.mbsyncrc
;; IMAPAccount <account>
;; Host <imap.host.tld>
;; Port <993>
;; User <email username>
;; PassCmd "pass <account>"
;; SSLType IMAPS
;; CertificateFile /etc/ssl/certs/ca-certificates.crt

;; IMAPStore <account>-remote
;; Account <account>

;; MaildirStore <account>-local
;; SubFolders Verbatim
;; Path ~/Mail/<account>/
;; Inbox ~/Mail/<account>/Inbox
;; Trash ~/Mail/<account>/Trash

;; Channel <account>
;; Far :<account>-remote:
;; Near :<account>-local:
;; ;; Patterns are the names of your mail folders. You can also use * for all of your mail folders.
;; Patterns Archive Drafts SentItems DeletedItems JunkEmail INBOX
;; SyncState *
;; Create Both
;; Expunge Both
;; CopyArrivalDate yes
;; Sync All
;; ===========================================================================
;; Setting up the mail database, execute this on the command line
;; mbsync -a
;; mu init --maildir=~/.mail \
;;         --my-address=user1@email.com \
;;         --my-address=user2@email.com
;; mu index
;; ===========================================================================

;; ===========================================================================
;; To send email
;; ===========================================================================
;; create an ~/.authinfo.gpg file containing one line per email account
;; Format: (your date micght differ)
;; machine smtp-mail.outlook.com login <email> port 587 password <password>

;; ===========================================================================
;; To send email
;; ===========================================================================
;; Use the ~/.msmtprc configuration file with the following content:

;; # Set default values for all following accounts.
;; defaults
;; auth           on
;; tls            on
;; tls_trust_file /etc/ssl/certs/ca-certificates.crt
;; #logfile        ~/.msmtp.log

;; # echjansen@outlook.com
;; account        <account>
;; host           smtp-mail.outlook.com
;; port           587
;; from           <email>
;; user           <email> or <user>
;; passwordeval   "pass email/user"
;; from_full_name "User full name"

;;; Code:

;;; = mu4e - Emacs Mail Client
(use-package mu4e
  :ensure nil
  :commands (mu4e)
  :custom
  (mu4e-context-policy 'pick-first)
  (mu4e-confirm-quit nil)
  (mu4e-modeline-support t)
  (mu4e-search-show-threads nil)
  (mu4e-search-threads nil)
  (mu4e-thread-mode nil)
  (mu4e-use-fancy-chars t)
  (mu4e-search-result-limit 500)
  (mu4e-search-full t)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-headers-date-format "%Y-%m-%d")
  (mu4e-headers-time-format "%H:%M:%S")
  (mu4e-eldoc-support t)
  ;; Required for moving and deleting messages (IMAP)
  (mu4e-change-filenames-when-moving t)
  ;; Tell Emacs mu4e is the email agent
  (mail-user-agent 'mu4e-user-agent)
  ;; Synchronizing the mailbox
  (mu4e-get-mail-command "mbsync -a")
  ;; Headers for mail list
  (mu4e-headers-fields
   '((:human-date . 18)
     (:flags      . 8)
     (:size       . 10)
     (:from       . 40)
     (:thread-subject)))
  ;; Local mail directory
  (mu4e-maildir ~/.mail)
  ;; Automatically update mailbox every 10 minutes
  (mu4e-update-interval 600)
  ;; Sending mail
  ;;(message-send-mail-function 'smtpmail-send-it)              ; uses .authinfo
  (message-send-mail-function 'message-send-mail-with-sendmail) ; uses .msmtprc
  (sendmail-program "/usr/bin/msmtp")
  ;; (message-sendmail-f-is-evil t)
  ;; (message-sendmail-extra-arguments '("--read-envelope-from"))

  ;; Composing emails
  (mu4e-compose-format-flowed t)
  (message-kill-buffer-on-exit t)

  :config
  ;; Avoid 'd' deleted messages are flagged for expunge
  ;; Instead move 'd' messages to the Trash folder
  (setf (plist-get (alist-get 'trash mu4e-marks) :action)
        (lambda (docid msg target)
          (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
  ;; Configure one or more email accounts
  (setq mu4e-contexts
        `(
          ,(make-mu4e-context
            :name "ech"
            :enter-func (lambda ()
                          (mu4e-message "Entering 'ech' context")
                          (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                            (revert-buffer)))
            :leave-func (lambda ()
                          (mu4e-message "Leaving 'ech' context")
                          (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                            (revert-buffer)))
            :match-func
            (lambda
              (msg)
              (when msg
                (string-prefix-p "/ech" (mu4e-message-field msg :maildir))))
            :vars `(
                    (user-full-name             . ,(auth-source-pass-get "name" "email/ech"))
                    (user-mail-address          . ,(auth-source-pass-get "email" "email/ech"))
                    (mu4e-compose-signature     . (concat
                                                   "Kind regards,\n\n"
                                                   ,(auth-source-pass-get "name" "email/ech")
                                                   "\n"))
                    (mu4e-sent-folder           . "/ech/Sent")
                    (mu4e-drafts-folder         . "/ech/Drafts")
                    (mu4e-trash-folder          . "/ech/Deleted")
                    (mu4e-refile-folder         . "/ech/Archive")
                    (mu4e-junk-folder           . "/ech/Junk")
                    (mu4e-maildir-shortcuts     . ((:maildir "/ech/Inbox"   :key ?i :name "Inbox")
                                                   (:maildir "/ech/Archive" :key ?a :name "Archive")
                                                   (:maildir "/ech/Sent"    :key ?s :name "Sent")
                                                   (:maildir "/ech/Deleted" :key ?t :name "Trash")
                                                   (:maildir "/ech/Junk"    :key ?j :name "Junk")))))

          ,(make-mu4e-context
            :name "web"
            :enter-func (lambda ()
                          (mu4e-message "Entering 'web' context")
                          (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                            (revert-buffer)))
            :leave-func (lambda ()
                          (mu4e-message "Leaving 'web' context")
                          (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                            (revert-buffer)))
            :match-func
            (lambda
              (msg)
              (when msg
                (string-prefix-p "/web" (mu4e-message-field msg :maildir))))
            :vars `(
                    (user-full-name             . ,(auth-source-pass-get "name" "email/web"))
                    (mu4e-compose-signature     . (concat
                                                   "Kind regards,\n\n"
                                                   ,(auth-source-pass-get "name" "email/web")
                                                   "\n"))
                    (mu4e-sent-folder           . "/web/Sent")
                    (mu4e-drafts-folder         . "/web/Drafts")
                    (mu4e-trash-folder          . "/web/Deleted")
                    (mu4e-refile-folder         . "/web/Archive")
                    (mu4e-junk-folder           . "/web/Junk")
                    (mu4e-maildir-shortcuts     . ((:maildir "/web/Inbox"   :key ?i :name "Inbox")
                                                   (:maildir "/web/Archive" :key ?a :name "Archive")
                                                   (:maildir "/web/Sent"    :key ?s :name "Sent")
                                                   (:maildir "/web/Deleted" :key ?t :name "Trash")
                                                   (:maildir "/web/Junk"    :key ?j :name "Junk")))))

          ,(make-mu4e-context
            :name "franz"
            :enter-func (lambda ()
                          (mu4e-message "Entering 'franz' context")
                          (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                            (revert-buffer)))
            :leave-func (lambda ()
                          (mu4e-message "Leaving 'franz' context")
                          (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
                            (revert-buffer)))
            :match-func
            (lambda
              (msg)
              (when msg
                (string-prefix-p "/franz" (mu4e-message-field msg :maildir))))
            :vars `(
                    (user-full-name             . ,(auth-source-pass-get "name" "email/franz"))
                    (user-mail-address          . ,(auth-source-pass-get "email" "email/franz"))
                    (mu4e-compose-signature     . (concat
                                                   "Kind regards,\n\n"
                                                   ,(auth-source-pass-get "name" "email/franz")
                                                   "\n"))
                    (mu4e-sent-folder           . "/franz/Sent")
                    (mu4e-drafts-folder         . "/franz/Drafts")
                    (mu4e-trash-folder          . "/franz/Deleted")
                    (mu4e-refile-folder         . "/franz/Archive")
                    (mu4e-junk-folder           . "/franz/Junk")
                    (mu4e-maildir-shortcuts     . ((:maildir "/franz/Inbox"   :key ?i :name "Inbox")
                                                   (:maildir "/franz/Archive" :key ?a :name "Archive")
                                                   (:maildir "/franz/Sent"    :key ?s :name "Sent")
                                                   (:maildir "/franz/Deleted" :key ?t :name "Trash")
                                                   (:maildir "/franz/Junk"    :key ?j :name "Junk")))))
          )))

;;; = org-mime - Compose in org and convert to html
(use-package org-mime
  :commands (org-mime-htmlize
             org-mime-edit-mail-in-org-mode
             org-mime-org-buffer-htmlize
             org-mime-org-subtree-htmlize)
  :custom
  (org-mime-export-options '(;; Export options
                             :section-mumber nil
                             :with-author nil
                             :with-to nil))
  :hook
  (message-send . org-mime-confirm-when-no-multipart)
  (org-mime-html . (lambda ()
                     (org-mime-change-element-style
                      "pre" (format "color: %s background-color: %s padding: 0.5em;"
                      "#E6E1DC" "#232323")))))

(provide 'pure-email)
;;; pure-email.el ends here.
