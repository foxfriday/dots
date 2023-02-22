;;; set-mu.el --- Email settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
                   (add-to-list 'load-path (concat user-emacs-directory "pkgs/mu4e"))
                   (require 'mu4e))
(require 'evil)
(require 'defaults)

;;; mu4e
(with-eval-after-load 'mu4e
  (declare-function mu4e-action-view-in-browser "mu4e-view" (msg &optional args))

  (defvar extra-temp-email-dir "~/Downloads/tmp-mu/"
    "Location of temporary files for Emacs mu4e.")

  (defun extra-clean-temp-email-directory ()
    "Remove all files from DIR if possible."
    (when (file-directory-p extra-temp-email-dir)
      (let ((files (directory-files extra-temp-email-dir
                                    t
                                    directory-files-no-dot-files-regexp)))
        (dolist (file files)
          (condition-case nil
              (delete-file file)
            (error nil)))
        (condition-case nil
            (delete-directory extra-temp-email-dir)
          (error nil)))))

  (add-hook 'kill-emacs-hook 'extra-clean-temp-email-directory)

  ;; Additional actions
  (defun extra-move-temp-email-location (msg &optional args)
    "Move and rename temp file MSG to a new location with ARGS ignored."
    (let* ((temp (format-time-string (concat extra-temp-email-dir
                                             "%Y-%m-%dT%H:%M.html")))
           (name (read-string "File name: " temp))
           (file (replace-regexp-in-string (regexp-quote "file://") "" msg t t)))
      (if args (message "Additional optional argument was ignored when saving to HTML."))
      (rename-file file name)))

  (defun extra-email-to-pdf (msg &optional args)
    "Pdf temp file MSG to a new name with ARGS ignored."
    (let* ((async-shell-command-display-buffer nil)
           (temp (format-time-string "~/Downloads/%Y-%m-%dT%H:%M.pdf"))
           (name (read-string "File name: " temp))
           (html (replace-regexp-in-string (regexp-quote "file://") "" msg t t)))
      (if args (message "Additional optional argument was ignored when saving to PDF."))
      (async-shell-command (concat "pandoc " html " -o " name))))

  (defun extra-save-email-html (msg &optional skip-headers)
    "Save current MSG HTML-part.
If SKIP-HEADERS is set, do not show include message headers."
    (let* ((extra-temp-email-dir "~/Downloads/")
           (browse-url-browser-function  'extra-move-temp-email-location))
      (mu4e-action-view-in-browser msg skip-headers)))

  (defun extra-print-email-to-pdf (msg &optional skip-headers)
    "Save current MSG as a pdf if it includes an HTML-part.
If SKIP-HEADERS is set, do not show include message headers."
    (let* ((browse-url-browser-function  'extra-email-to-pdf))
      (mu4e-action-view-in-browser msg skip-headers)))

  (defun extra-open-email-in-browser (msg &optional skip-headers)
    "Show current MSG in browser if it includes an HTML-part.
This is a wrapper function for `mu4e-action-view-in-browser`
needed to get around snaps inability to access the temp
directory. If SKIP-HEADERS is set, do not show include message
headers. The variables `browse-url-browser-function',
`browse-url-handlers', and `browse-url-default-handlers'
determine which browser function to use."
    (let* ((temporary-file-directory extra-temp-email-dir))
      (unless (file-directory-p temporary-file-directory)
        (make-directory temporary-file-directory t))
      (mu4e-action-view-in-browser msg skip-headers)))

  (setq mu4e-view-actions '(("capture message"  . mu4e-action-capture-message)
                            ("view in browser"  . extra-open-email-in-browser)
                            ("download as html"  . extra-save-email-html)
                            ("print to PDF"  . extra-print-email-to-pdf)
                            ("show this thread" . mu4e-action-show-thread)))
  ;; Bindings
  (evil-set-initial-state 'mu4e-view-mode 'normal)
  (evil-set-initial-state 'mu4e-headers-mode 'normal)
  (evil-set-initial-state 'mu4e-compose-mode 'normal)

  (evil-define-key 'normal mu4e-headers-mode-map
    "q"  'mu4e~headers-quit-buffer
    "J"  'mu4e~headers-jump-to-maildir
    "C"  'mu4e-compose-new
    "E"  'mu4e-compose-edit
    "F"  'mu4e-compose-forward
    "R"  'mu4e-compose-reply
    "j"  'mu4e-headers-next
    "k"  'mu4e-headers-prev
    ";"  'mu4e-context-switch
    (kbd "RET") 'mu4e-headers-view-message
    "s"  'mu4e-search
    "x"  'mu4e-mark-execute-all
    "a"  'mu4e-headers-action
    "A"  'mu4e-headers-mark-for-action
    "m"  'mu4e-headers-mark-for-move
    "D"  'mu4e-headers-mark-for-delete
    "d"  'mu4e-headers-mark-for-trash
    "u"  'mu4e-headers-mark-for-unmark
    "U"  'mu4e-mark-unmark-all
    "[[" 'mu4e-headers-prev-unread
    "]]" 'mu4e-headers-next-unread
    "gv" 'mu4e-select-other-view)

  (evil-define-key 'normal mu4e-view-mode-map
    "gg" 'mu4e-compose-goto-top
    "G"  'mu4e-compose-goto-bottom
    "]]" 'mu4e-view-headers-next
    "[[" 'mu4e-view-headers-prev
    ">"  'mu4e-view-headers-next-unread
    "<"  'mu4e-view-headers-prev-unread
    "a"  'mu4e-view-action
    "A"  'mu4e-view-mime-part-action
    "C"  'mu4e-compose-new
    "E"  'mu4e-compose-edit
    "F"  'mu4e-compose-forward
    "R"  'mu4e-compose-reply
    "H"  'mu4e-view-toggle-html
    "go" 'mu4e-view-go-to-url
    "m"  'mu4e-view-mark-for-move
    "D"  'mu4e-view-mark-for-delete
    "d"  'mu4e-view-mark-for-trash
    "u"  'mu4e-view-unmark
    "U"  'mu4e-view-unmark-all
    "q"  'mu4e~view-quit-buffer)

  (evil-define-key 'normal mu4e-compose-mode-map
    "ga" 'mml-attach-file
    "gg" 'mu4e-compose-goto-top
    "G"  'mu4e-compose-goto-bottom
    "ZQ" 'mu4e-message-kill-buffer
    "q"  'mu4e-message-kill-buffer
    "ZZ" 'message-send-and-exit)

  ; Otherwise includes related messages (e.g. sent)
  (setq mu4e-headers-include-related nil)

  ;;; Replies
  (add-hook 'mu4e-compose-pre-hook 'evil-insert-state)
  (add-hook 'mu4e-compose-pre-hook
            (lambda ()
              (if mu4e-compose-parent-message
                  (setq-local user-mail-address
                              (plist-get (car-safe (mu4e-message-field mu4e-compose-parent-message :to)) :email)))))
  ;; Settings
  ; prefer text if available, images least. Also stop loading attached images.
  (setq mm-discouraged-alternatives (list "image/.*" "text/html" "text/richtext")
        gnus-inhibit-images t
        mu4e-get-mail-command "mbsync -a"
        mu4e-attachment-dir "~/Downloads"
        mu4e-search-threads nil
        mu4e-compose-dont-reply-to-self t
        mu4e-change-filenames-when-moving t
        mu4e-view-scroll-to-next nil))

(provide 'set-mu)
;;; set-mu.el ends here
