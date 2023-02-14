;;; set-proced.el --- Set up proced (htop) -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure proced

;;; Code:
(eval-when-compile (require 'proced))
(require 'evil)

;;; proced
(setq-default proced-auto-update-flag t)
(evil-set-initial-state 'proced-mode 'motion)

(with-eval-after-load 'proced
  (add-to-list 'proced-filter-alist '(python (comm . "^python")))
  (setq proced-auto-update-interval 1
        proced-enable-color-flag t)
  (evil-define-key 'motion proced-mode-map
    "C"  'proced-mark-children
    "F"  'proced-format-interactive
    "M"  'proced-mark-all
    "P"  'proced-mark-parents
    "T"  'proced-toggle-tree
    "U"  'proced-unmark-all
    "d"  'proced-mark
    "f"  'proced-filter-interactive
    "g"  'revert-buffer
    "k"  'proced-send-signal
    "m"  'proced-mark
    "n"  'next-line
    "p"  'previous-line
    "q"  'quit-window
    "r"  'proced-renice
    "t"  'proced-toggle-marks
    "u"  'proced-unmark
    "x"  'proced-send-signal
    "h"  'highlight-phrase
    "u"  'proced-undo
    "sS" 'proced-sort-interactive
    "sc" 'proced-sort-pcpu
    "sm" 'proced-sort-pmem
    "sp" 'proced-sort-pid
    "ss" 'proced-sort-start
    "st" 'proced-sort-time
    "su" 'proced-sort-user))
;;; End
(provide 'set-proced)
;;; set-proced.el ends here
