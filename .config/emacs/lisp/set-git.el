;;; set-git.el --- Base Emacs set up -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure all git related packages.
;; - diff-hl

;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/")))
(require 'evil)
(require 'diff-hl)
(require 'defaults)

;;; Diff-Hl (gitgutter)
(unless is-gui (add-hook 'diff-hl-mode-hook 'diff-hl-margin-mode))

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'diff-hl-mode 'diff-hl-flydiff-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
(add-hook 'vc-dir-mode-hook 'diff-hl-dir-mode)
(setq diff-hl-show-staged-changes nil)

(evil-define-key 'normal diff-hl-mode-map
  "]c" 'diff-hl-next-hunk
  "[c" 'diff-hl-previous-hunk)

(evil-define-key 'normal diff-hl-inline-popup-transient-mode-map
  "q" 'diff-hl-inline-popup-hide
  (kbd "<escape>") 'diff-hl-inline-popup-hide
  "j" 'diff-hl-inline-popup--popup-down
  "k" 'diff-hl-inline-popup--popup-up
  (kbd "C-f") 'diff-hl-inline-popup--popup-pagedown
  (kbd "C-b") 'diff-hl-inline-popup--popup-pageup)

(evil-define-key 'normal diff-hl-show-hunk-map
  "p" 'diff-hl-show-hunk-previous
  "n" 'diff-hl-show-hunk-next
  "c" 'diff-hl-show-hunk-copy-original-text
  "r" 'diff-hl-show-hunk-revert-hunk)
;;; End
(provide 'set-git)
;;; set-git.el ends here
