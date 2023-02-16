;;; set-evil.el --- Evil bindings -*- lexical-binding: t; -*-

;;; Commentary:

;; Manages the core vim related bindings in the following libraries.
;; A leader is set with a prefix command.
;; - evil
;; - repeat-mode

;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/")))
(require 'evil)
(require 'extra)
(require 'defaults)

;;; Evil
(evil-mode 1)

(unless is-gui
  (add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode)))
  (add-hook 'evil-insert-state-exit-hook (lambda () (hl-line-mode -1))))

(evil-define-operator extra-evil-find (file)
  "Open FILE, taking wild cards."
  :repeat nil
  (interactive "<f>")
  (if file (find-file file t) (call-interactively 'find-file)))
(evil-ex-define-cmd "find" 'extra-evil-find)

(defvar extra-evil-mode-line
  '((:eval (propertize evil-mode-line-tag 'face '(:foreground "red")))
    (vc-mode vc-mode)  ;; git info
    " (" mode-line-modified ") "
    mode-line-buffer-identification "|"
    mode-line-percent-position
    mode-line-position
    mode-name "|"
    mode-line-misc-info)
  "Mode-line configuration.")

(setq-default evil-mode-line-format nil
              mode-line-position '("|%l:%c|")
              mode-line-format extra-evil-mode-line)

;;; Bindings
(evil-define-key 'visual 'global "S" 'extra-surround)
(evil-define-key 'visual 'global "C" 'capitalize-region)
(evil-define-key 'normal 'global "gf" 'find-file-at-point)
(keymap-global-set "M-Q" 'extra-unfill-paragraph)
(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "C-c c" 'org-capture)

;;; Repeat Mode
(repeat-mode 1)

(defvar window-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" 'evil-window-left)
    (define-key map "l" 'evil-window-right)
    (define-key map "k" 'evil-window-up)
    (define-key map "j" 'evil-window-down)
    map)
  "Keymap to repeat `evil-window-left' and related commands with `repeat-mode'.")

(put 'evil-window-left 'repeat-map 'window-navigation-repeat-map)
(put 'evil-window-right 'repeat-map 'window-navigation-repeat-map)
(put 'evil-window-up 'repeat-map 'window-navigation-repeat-map)
(put 'evil-window-down 'repeat-map 'window-navigation-repeat-map)

;;; Leader
(define-prefix-command 'my-leader-map)
(keymap-set evil-motion-state-map "SPC" 'my-leader-map)
(keymap-set evil-normal-state-map "SPC" 'my-leader-map)

(evil-define-key nil my-leader-map
  ;; projects
  "b"  'switch-to-buffer
  "B"  'project-switch-to-buffer
  "pf" 'project-find-file
  "ps" 'project-shell-command
  ;; buffers
  "kd" (lambda () (interactive) (extra-kill-buffers-in-mode 'dired-mode))
  "kk" 'kill-this-buffer
  "kK" (lambda () (interactive) (kill-this-buffer) (delete-window))
  "kp" 'project-kill-buffers
  (kbd "k <up>") (lambda () (interactive) (extra-close-other-buffer "k"))
  (kbd "k <down>") (lambda () (interactive) (extra-close-other-buffer "j"))
  (kbd "k <left>") (lambda () (interactive) (extra-close-other-buffer "h"))
  (kbd "k <right>") (lambda () (interactive) (extra-close-other-buffer "l"))
  (kbd "K <up>") (lambda () (interactive) (extra-kill-other-buffer "k"))
  (kbd "K <down>") (lambda () (interactive) (extra-kill-other-buffer "j"))
  (kbd "K <left>") (lambda () (interactive) (extra-kill-other-buffer "h"))
  (kbd "K <right>") (lambda () (interactive) (extra-kill-other-buffer "l"))
  ;; search/switch
  "sd" 'extra-search-dir
  "sD" 'extra-search-to-dired
  "sn" (lambda () (interactive) (extra-open-in my-work-notes "\\.org$\\|\\.tex$\\|\\.gpg$"))
  "sq" (lambda () (interactive) (extra-open-in my-notes "\\.org$\\|\\.tex$\\|\\.gpg$"))
  "sw" 'webjump
  "sW" 'dictionary-search
  "/"  'occur
  ;; diff-hl gitgutter
  "hs" 'diff-hl-stage-current-hunk
  "hU" 'diff-hl-unstage-file
  "hv" 'diff-hl-show-hunk ; hunk view
  ;; other
  ">"  'scroll-other-window
  "<"  'scroll-other-window-down
  "n"  'extra-narrow
  ","  'evil-ex-nohighlight
  "l"  'org-store-link
  "i"  'imenu
  "S"  'extra-surround
  "C"  'world-clock
  "tn" 'extra-toggle-line-numbering
  "tf" 'toggle-frame-fullscreen)
;;; End
(provide 'set-evil)
;;; set-evil.el ends here
