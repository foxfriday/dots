;;; set-compile.el --- Programming tools set up -*- lexical-binding: t; -*-

;;; Commentary:
;; Set up compilation mode

;;; Code:
(require 'evil)

;;; Compilation
(with-eval-after-load 'compile
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook (lambda () (ansi-color-apply-on-region (point-min) (point-max))))
  (evil-define-key 'motion compilation-mode-map
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

(provide 'set-compile)
;;; set-compile.el ends here
