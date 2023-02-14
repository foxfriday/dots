;;; set-term.el --- Dired set up -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure terminal emulators.
;; - term

;;; Code:
(eval-when-compile (require 'term))
(require 'evil)

;;; Term
(with-eval-after-load 'term ;; access bindings with C-c
  (evil-make-overriding-map term-mode-map 'insert)
  (evil-define-key 'normal term-raw-map
    "]]" 'term-next-prompt
    "[[" 'term-previous-prompt)
  (evil-define-key '(insert motion normal) term-raw-map
    (kbd "C-w h") 'evil-window-left
    (kbd "C-w l") 'evil-window-right
    (kbd "C-w k") 'evil-window-up
    (kbd "C-w j") 'evil-window-down))
;;; End
(provide 'set-term)
;;; set-term.el ends here
