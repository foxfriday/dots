;;; set-treesit.el --- Set native tree sitter for Emacs 29+ -*- lexical-binding: t; -*-

;;; Commentary:
; Require Emacs 29 and the tree sitter modules.
; To see available modes run the following function
; (taken from https://github.com/renzmann/treesit-auto):
;
; (defun treesit-auto--available-modes ()
;   "Build a list of all modes ending with `-ts-mode' as strings."
;   (let ((result '()))
;     (mapatoms (lambda (elt)
;                 (when-let* ((is-func (functionp elt))
;                             (name (symbol-name elt))
;                             (match (string-match "-ts-mode$" name))
;                             (no-internals (not (string-match "--" name))))
;                   (push (intern name) result))))
;     result))

;;; Code:

(setq treesit-extra-load-path (list (concat user-emacs-directory "ts/")))
(setq major-mode-remap-alist '((c++-mode . c++-ts-mode)
                               (c-mode . c-ts-mode)
                               (c-or-c++-mode . c-or-c++-ts-mode)
                               (conf-toml-mode . toml-ts-mode)
                               (csharp-mode . csharp-ts-mode)
                               (css-mode . css-ts-mode)
                               (java-mode . java-ts-mode)
                               (js-json-mode . json-ts-mode)
                               (python-mode . python-ts-mode)
                               (ruby-mode . ruby-ts-mode)
                               (sh-mode . bash-ts-mode)))
; tree-sitter only modes
(add-to-list 'auto-mode-alist '("CMakeLists\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))

(provide 'set-treesit)
;;; set-treesit.el ends here
