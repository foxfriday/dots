;;; set-dired.el --- Dired set up -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired defaults
;; WDired is the editing mode

;;; Code:
(eval-when-compile (require 'dired)
                   (require 'dired-x))
(require 'evil)
;;; Dired
(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(with-eval-after-load 'wdired
  (evil-define-key 'normal wdired-mode-map
    "ZQ" 'wdired-abort-changes
    "ZZ" 'wdired-finish-edit
    (kbd "<escape>") 'wdired-exit))

(with-eval-after-load 'dired
  (setq delete-by-moving-to-trash nil
        dired-listing-switches "-alh"
        dired-dwim-target t
        ; hide in `dired-omit-mode`
        dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"
        ; dired flag garbage files
        dired-garbage-files-regexp (rx "." (or "aux"
                                               "auxlock"
                                               "elc"
                                               "out"
                                               "log"
                                               "toc"
                                               "synctex.gz")
                                       string-end))

  (keymap-set dired-mode-map "SPC" nil)
  (evil-define-key 'normal dired-mode-map
    ; actions
    "C"  'dired-do-copy
    "D"  'dired-do-delete
    "R"  'dired-do-rename
    "!"  'dired-do-shell-command
    "&"  'dired-do-async-shell-command
    "A"  'gnus-dired-attach
    "F"  'dired-flag-garbage-files
    (kbd my-return) 'dired-find-file
    "go" 'dired-find-file-other-window
    "gO" (lambda () (interactive) (extra-open-out (dired-get-file-for-visit)))
    ; mark
    "m"  'dired-mark
    "d"  'dired-flag-file-deletion
    "u"  'dired-unmark
    "x"  'dired-do-flagged-delete
    "U"  'dired-unmark-all-marks
    ; navigation
    "[[" 'dired-prev-dirline
    "]]" 'dired-next-dirline
    "<"  'dired-up-directory
    ; common
    "gr" 'revert-buffer
    "O"  'dired-omit-mode
    "o"  'dired-sort-toggle-or-edit
    "q"  'kill-this-buffer))
;;; End
(provide 'set-dired)
;;; set-dired.el ends here
