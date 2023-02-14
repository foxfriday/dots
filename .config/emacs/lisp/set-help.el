;;; set-help.el --- Help modes -*- lexical-binding: t; -*-

;;; Commentary:
;; Set help and documentation.
;; - man
;; - help-mode (emacs manual)
;; - devdoc
;;; Code:
(require 'evil)

;;; Man
(with-eval-after-load 'man
  (evil-define-key 'motion Man-mode-map
    "]]" 'Man-next-section
    "[[" 'Man-previous-section
    "gs" 'Man-goto-section
    ">"  'Man-follow-manual-reference))

;;; Help
(with-eval-after-load 'help-mode
  (evil-define-key 'motion help-mode-map
    "]]" 'help-go-forward
    "[[" 'help-go-back
    "gd" 'help-view-source
    "q"  'kill-this-buffer))

;;; Info
(eval-when-compile (defvar Info-mode-map))
(with-eval-after-load 'info
  (evil-make-overriding-map Info-mode-map 'motion))

;;; Devdocs
(add-hook 'bash-ts-mode-hook
          (lambda () (setq-local devdocs-current-docs '("bash"))))
(add-hook 'c-ts-base-mode-hook
          (lambda () (setq-local devdocs-current-docs '("cpp" "eigen3"))))
(add-hook 'LaTeX-mode-hook
          (lambda () (setq-local devdocs-current-docs '("latex"))))
(add-hook 'makefile-mode-hook
          (lambda () (setq-local devdocs-current-docs '("gnu_make"))))
(add-hook 'python-base-mode-hook
          (lambda () (setq-local devdocs-current-docs '("python~3.10" "pandas~1" "numpy~1.23"))))
(add-hook 'rust-ts-mode
          (lambda () (setq-local devdocs-current-docs '("rust"))))

(keymap-global-set "C-h D" 'devdocs-lookup)

(with-eval-after-load 'devdocs
  (evil-define-key 'normal devdocs-mode-map
    "[[" 'devdocs-previous-page
    "]]" 'devdocs-next-page
    "<"  'devdocs-go-back
    ">"  'devdocs-go-forward
    "gs" 'devdocs-lookup
    "q"  'kill-this-buffer))

;;; End
(provide 'set-help)
;;; set-help.el ends here
