;;; set-mail.el --- Email settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Set up email settings.
;; - imenu for messages
;; - sendmail
;; - message and ecomplete
;; - org-mime

;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
                   (require 'smtpmail)
                   (require 'message)
                   (require 'org-mime))
(require 'evil)
(require 'defaults)
;;; imenu
(defvar extra-imenu-mail-expressions '(("Subject" "^Subject: *\\(.*\\)" 1)
                                       ("Cc" "^C[Cc]: *\\(.*\\)" 1)
                                       ("To" "^To: *\\(.*\\)" 1)
                                       ("From" "^From: *\\(.*\\)" 1)
                                       ("Body" "^--text follows*\n\\(.*\\)" 1))
  "Additional `imenu` matches for message mode.")
(add-hook 'message-mode-hook
          (lambda () (setq imenu-generic-expression extra-imenu-mail-expressions)))

;;; message, ecomplete, sendmail and smtpmail (optional)
(defvar extra-mail-aliases nil
  "Association list mapping emails to user names.")

(with-eval-after-load 'message
  (require 'sendmail)
  (require 'org-mime)

  ;; sendmail
  (defun extra-setup-smtp-mail ()
    "Set Emacs' smtp settings -not used with msmtp."
    (interactive)
    (let* ((mail (nth 1 (mail-extract-address-components
		        (message-fetch-field "from"))))
           (user (cdr (assoc mail extra-mail-aliases)))
           (user (if user user mail))
           (provider (nth 1 (string-split user "@"))))
      (cond ((string= provider "outlook.com")
             (setq smtpmail-smtp-user user
                   smtpmail-smtp-server "smtp.office365.com"
                   smtpmail-stream-type 'starttls
                   smtpmail-smtp-service 587))
            ((string= provider "mailbox.org")
             (setq smtpmail-smtp-user user
                   smtpmail-smtp-server "smtp.mailbox.org"
                   smtpmail-stream-type 'ssl
                   smtpmail-smtp-service 465))
            ((string= provider "icloud.com")
             (setq smtpmail-smtp-user user
                   smtpmail-smtp-server "smtp.mailbox.org"
                   smtpmail-stream-type 'starttls
                   smtpmail-smtp-service 587))
            (t (warn "Unable to set smtpmail configuration")))))

  (if has-msmtp (setq sendmail-program has-msmtp
                      send-mail-function 'sendmail-send-it)
    (progn (setq send-mail-function 'smtpmail-send-it
                 smtpmail-queue-dir "~/Mail/queue"
                 smtpmail-servers-requiring-authorization ".*")
           (add-hook 'message-send-hook 'extra-setup-smtp-mail)))
  (setq mail-specify-envelope-from t
        mail-envelope-from 'header)

  ;; message
  (setq message-signature "M. Rincón"
        message-directory "~/Mail"
        message-auto-save-directory "~/Mail/drafts"
        ; place forward messages after signature
        message-forward-before-signature nil
        message-fill-column nil
        message-kill-buffer-on-exit t
        message-sendmail-envelope-from 'header
        message-mail-alias-type 'ecomplete
        message-self-insert-commands nil
        message-expand-name-standard-ui t
        ; ensure I can later read own messages
        mml-secure-openpgp-encrypt-to-self t)

  ;; bindings
  (evil-define-key 'normal message-mode-map
    "R"  'message-reply
    "F"  'message-forward
    "gt" 'message-goto-to
    "gc" 'message-goto-cc
    "gs" 'message-goto-subject
    "gb" 'message-goto-body
    "ge" 'org-mime-edit-mail-in-org-mode
    "gi" 'message-insert-or-toggle-importance
    "ga" 'mml-attach-file
    "gS" 'mml-secure-message-sign
    "gE" 'mml-secure-message-encrypt
    "gV" (lambda () (interactive) (mml-preview t))
    "ZD" 'message-dont-send
    "ZZ" 'message-send-and-exit
    "ZQ" 'message-kill-buffer)

  (ecomplete-setup))

;;; org-mime
(defun change-html-element-style (element style)
  "Set <ELEMENT> elements in exported html with new default html STYLE."
  (while (re-search-forward (format "<%s" element) nil t)
    (replace-match (format "<%s style=\"%s\"" element style))))

(defun change-html-class-style (class style)
  "CLASS is used for new default html STYLE in exported html."
  (while (re-search-forward (format "class=\"%s\"" class) nil t)
    (replace-match (format "class=\"%s\" style=\"%s\"" class style))))

(with-eval-after-load 'org-mime
  (defun extra-org-mime-css ()
    (change-html-element-style ; dark background on code
     "pre"
     "color: #E6E1DC; background-color: #232323; padding: 0.5em;")
    (change-html-element-style ; offset blockquotes
     "blockquote"
     "border-left: 2px solid gray; padding-left: 4px;"))
  (add-hook 'org-mime-html-hook 'extra-org-mime-css)
  (setq org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil)))

(provide 'set-mail)
;;; set-mail.el ends here
