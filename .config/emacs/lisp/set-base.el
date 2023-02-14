;;; set-base.el --- Emacs' defaults. -*- lexical-binding: t; -*-

;;; Commentary
;; A set of proffered defaults for core Emacs.

;;; Code:

(setq-default auto-save-default nil
              create-lockfiles nil
              describe-bindings-outline t
              fill-column 90
              frame-resize-pixelwise t
              help-enable-symbol-autoload t
              indent-tabs-mode nil
              line-spacing 0.2
              load-prefer-newer t
              make-backup-files nil
              next-error-message-highlight t
              prettify-symbols-unprettify-at-point 'right-edge
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t
              ring-bell-function 'ignore
              select-enable-clipboard t
              sentence-end-double-space nil
              use-short-answers t
              epg-pinentry-mode 'loopback)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(show-paren-mode 1)
(blink-cursor-mode -1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(keymap-global-unset "C-x C-d") ;; no list directories

(provide 'set-base)
;;; set-base.el ends here
