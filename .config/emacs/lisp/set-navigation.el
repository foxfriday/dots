;;; set-navigation.el --- Base Emacs set up -*- lexical-binding: t; -*-

;;; Commentary:

;; Code navigation, including searches with grep
;; Managed libraries:
;; - outline-mode
;; - imenu
;; - occur
;; - grep
;; - ibuffer

;;; Code:
(eval-when-compile (require 'ibuf-ext))
(require 'evil)

;;; Outline
(defun set-outline-minor-mode ()
  (outline-minor-mode 1)
  (cond ((derived-mode-p 'conf-mode) (setq-local outline-regexp "^### *"))
        ((derived-mode-p 'lisp-data-mode) (setq-local outline-regexp "^;;; *"))
        (t nil)))

(add-hook 'prog-mode-hook 'set-outline-minor-mode)
(add-hook 'LaTeX-mode-hook 'set-outline-minor-mode)
(add-hook 'conf-mode-hook 'set-outline-minor-mode)

(with-eval-after-load 'outline
  (evil-define-key 'normal outline-minor-mode-map
    "[[" 'outline-previous-visible-heading
    "]]" 'outline-next-visible-heading
    "<"  'outline-backward-same-level
    ">"  'outline-forward-same-level
    (kbd "M-h") 'outline-promote
    (kbd "M-j") 'outline-move-subtree-down
    (kbd "M-k") 'outline-move-subtree-up
    (kbd "M-l") 'outline-demote))

;;; Imenu
(add-hook 'lisp-data-mode-hook
          (lambda ()
            (add-to-list 'imenu-generic-expression '("H1" "^;;; \\(.*\\)" 1))
            (add-to-list 'imenu-generic-expression '("H2" "^;;;; \\(.*\\)" 1))))
(add-hook 'conf-mode-hook
          (lambda ()
            (add-to-list 'imenu-generic-expression '("H1" "^### \\(.*\\)" 1))))
(add-hook 'bash-ts-mode-hook
          (lambda ()
            (add-to-list 'imenu-generic-expression '("H1" "^### \\(.*\\)" 1))))

;;; Occur
(evil-set-initial-state 'occur-mode 'motion)
(evil-set-initial-state 'occur-edit-mode 'normal)

(with-eval-after-load 'replace
  (evil-define-key 'motion occur-mode-map
    (kbd my-return) 'occur-mode-goto-occurrence
    "go" 'occur-mode-goto-occurrence-other-window
    "f"  'next-error-follow-minor-mode
    "ge" 'occur-edit-mode ;; edit seems unable to add to end of line
    "q"  'kill-this-buffer)
  (evil-define-key 'normal occur-edit-mode-map
    "ZZ" 'occur-cease-edit))

;;; Grep
(with-eval-after-load 'grep
  ;; no native edit, the package wgrep can add one if needed
  ;; grep mode inherits from compilation mode, so use functions
  (evil-define-key 'motion grep-mode-map
    (kbd my-return) 'compile-goto-error
    "]e" 'compilation-next-error
    "go" 'compilation-display-error
    "G"  'end-of-buffer
    "gr" 'recompile
    "]]" 'next-error-no-select
    "[[" 'previous-error-no-select
    "<"  'compilation-previous-file
    ">"  'compilation-next-file
    "f"  'next-error-follow-minor-mode
    "ZZ" 'kill-compilation
    "q"  'kill-this-buffer))

;;; ibuffer
(with-eval-after-load 'ibuffer
  (keymap-set ibuffer-mode-map "q" 'kill-this-buffer)
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-default-sorting-mode 'major-mode))
;;; End
(provide 'set-navigation)
;;; set-navigation.el ends here
