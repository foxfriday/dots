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

(defun extra-vc-install (path &optional name islocal)
  "Install NAME from PATH."
  (let* ((dname (if name name (file-name-base path)))
         (iname (intern dname)))
    (unless (package-installed-p iname)
      (if islocal
	(package-vc-install-from-checkout path dname)
        (package-vc-install path iname)))))

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
(extra-vc-install "~/Repos/emacs/date-functions" "date-functions" t)
(extra-vc-install "~/Repos/emacs/extra" "extra" t)
(extra-vc-install "~/Repos/emacs/ink" "ink" t)
(extra-vc-install "~/Repos/emacs/office" "office" t)

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
