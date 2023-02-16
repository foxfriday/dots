;;; -*- lexical-binding: t; -*-

;;; Helpers
(defun extra-path (dir)
  "Add DIR to path."
  (if (file-directory-p dir)
    (add-to-list 'load-path dir)))

(defun extra-install (pkg)
  "Install PKG."
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defun extra-github-install (repo dest &optional branch)
  "Clone BRANCH of REPO in DEST and install."
  (let* ((remote (concat "https://github.com/" repo ".git"))
         (name (nth 1 (string-split repo "/")))
         (pkg (intern name))
         (local (file-name-concat (expand-file-name dest) name))
         (branch (if branch branch "main")))
    (unless (file-directory-p local)
      (vc-clone remote 'Git local branch))
    (unless (package-installed-p pkg)
      (package-vc-install-from-checkout local pkg))))

;;; Paths
(extra-path (concat user-emacs-directory "lisp/"))
(extra-path (concat user-emacs-directory "pkgs/mu4e"))
(extra-path (concat user-emacs-directory "pkgs/ledger"))
(setq custom-file (concat user-emacs-directory "custom.el"))
;;; Packages
(extra-install 'auctex)
(extra-install 'cape)
(extra-install 'corfu)
(extra-install 'corfu-terminal)
(extra-install 'devdocs)
(extra-install 'diff-hl)
(extra-install 'evil)
(extra-install 'goto-chg)
(extra-install 'htmlize)
(extra-install 'org-mime)
(extra-install 'yasnippet)
(extra-github-install "foxfriday/extra" "~/Repos/emacs")
(extra-github-install "foxfriday/ink" "~/Repos/emacs")
(extra-github-install "foxfriday/office" "~/Repos/emacs")
(extra-github-install "foxfriday/date-functions" "~/Repos/emacs")

(package-initialize)

;;; Configure
;; These need to be set before loading Evil
(setq-default evil-want-keybinding nil
              evil-search-module 'evil-search
              evil-undo-system 'undo-redo)
(require 'defaults)
(require 'set-base)
(require 'set-calendar)
(require 'set-code)
(require 'set-compile)
(require 'set-completions)
(require 'set-dired)
(require 'set-evil)
(require 'set-git)
(require 'set-help)
(require 'set-mail)
(require 'set-navigation)
(require 'set-proced)
(require 'set-prose)
(require 'set-term)
(require 'set-tex)
(require 'set-treesit)
(require 'set-web)
(if is-gui (require 'set-theme))
(if is-gui (require 'set-images))
(when has-mu
  (require 'set-mu nil t)
  (unless (fboundp 'mu4e) (autoload 'mu4e "mu4e" "Email" t)))
(when has-ledger
  (require 'set-ledger nil t)
  (unless (fboundp 'ledger-mode) (autoload 'ledger-mode "ledger-mode" "Ledger mode" t)))
(unless (fboundp 'org-store-link) (autoload 'org "org" "Store org link" t))
(with-eval-after-load 'org (require 'set-org))

;;; local
(let ((local (concat user-emacs-directory "local")))
  (if (file-exists-p local) (load local)))
