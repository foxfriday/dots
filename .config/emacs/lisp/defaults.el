;;; defaults.el --- Set up paths, names, etc. -*- lexical-binding: t; -*-

;;; Commentary:

;; Default values, maybe system dependent.

;;; Code:

;; Capabilities
(defvar has-mu (eval-when-compile (executable-find "mu"))
  "Non-nil if system has mu the path.")

(defvar has-ledger (eval-when-compile (executable-find "ledger"))
  "Non-nil if system has ledger in the path.")

(defvar has-msmtp (eval-when-compile (executable-find "msmtp"))
  "Non-nil if system has msmtp in path and use to send emails.")

(defvar is-mac (eval-when-compile (eq system-type 'darwin))
  "Non-nil if on osX.")

(defvar is-gui (display-graphic-p)
  "Non-nil if running on a GUI.")

(defvar my-return (if is-gui "<return>" "RET")
  "Do the right thing when pressing <RET> in terminal.")

;; Defaults paths
(defvar my-org-files (eval-when-compile
                         (car (string-split
                               (shell-command-to-string "find ~ -type d -name org-files")
                               "\n"
                               t)))
  "File with a list of paths to loose org notes.")

(defvar my-agenda (eval-when-compile
                    (car (string-split
                          (shell-command-to-string "find ~ -type f -name agenda-files")
                          "\n"
                          t)))
  "File with a list of paths to org agenda files.")

(defvar my-work-notes "~/Repos/notes/cs/"
  "Directory with work notes files.")

(defvar my-diary (eval-when-compile
                    (car (string-split
                          (shell-command-to-string "find ~ -type f -name emacs.diary")
                          "\n"
                          t)))
  "Calendar diary location.")

(defvar diary-org (eval-when-compile
                    (car (string-split
                          (shell-command-to-string "find ~ -type f -name 'diary.org*'")
                          "\n"
                          t)))
  "Diary location.")

(defvar my-bibliography (eval-when-compile
                          (string-split
                           (shell-command-to-string "find ~ -type f -name '*.bib'")
                           "\n"
                           t))
  "Location of default bibliography files.")

;; Check requirements
(eval-when-compile
  (unless (executable-find "ltex-ls") (warn "Ltex-ls is missing from path."))
  (unless (executable-find "shellcheck") (warn "Shellcheck is missing from path."))
  (unless (executable-find "ruff") (warn "The ruff linter is missing from path."))
  (unless (executable-find "chktex") (warn "The chktex is missing from path.")))

(provide 'defaults)
;;; defaults.el ends here
