;;; set-completions.el --- Completions mechanisms -*- lexical-binding: t; -*-

;;; Commentary:

;; Completion mechanisms in the minibuffer and main buffer. All are enabled
;; by default.
;; Managed libraries:
;; - icomplete (fido)
;; - corfu and corfu-terminal
;; - yasnippets
;; - abbrev

;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/")))
(require 'icomplete)
(require 'defaults)
(require 'corfu)
(require 'corfu-terminal)
(require 'yasnippet)

;;; Fido Mode
; Otherwise not all options fit
(add-hook 'minibuffer-mode-hook (lambda () (setq-local line-spacing 0)))
(add-hook 'after-init-hook 'fido-vertical-mode)

(defun extra-describe-icomplete ()
  "Describe current `icomplete` selection."
  (interactive)
  (let* ((type (icomplete--category))
         (selection (intern-soft (car completion-all-sorted-completions))))
    (if selection
        (cond ((eq type 'command) (describe-function selection))
              ((eq type 'symbol-help) (describe-symbol selection))
              (t (message (concat "No help implemented for " (symbol-name type)))))
      (message "Not a valid selection."))))
; bindings
(keymap-set icomplete-fido-mode-map "C-a" 'extra-describe-icomplete)
(keymap-set icomplete-fido-mode-map "<right>" 'icomplete-fido-ret)
(keymap-set icomplete-fido-mode-map "<left>" 'icomplete-fido-backward-updir)
(keymap-set icomplete-fido-mode-map "C-x" 'icomplete-fido-exit)
(keymap-set icomplete-fido-mode-map "<escape>" 'abort-minibuffers)
(set-face-attribute 'icomplete-selected-match nil :underline t)
(setq completions-detailed t)
;;; Corfu
(setq corfu-auto t
      corfu-quit-no-match 'separator)
(add-hook 'completion-at-point-functions 'cape-file)
(add-hook 'completion-at-point-functions 'cape-abbrev)
(global-corfu-mode)
(unless is-gui (corfu-terminal-mode +1))

;;; Snippets
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'org-mode-hook 'yas-minor-mode)
(add-hook 'after-init-hook 'yas-reload-all)

;;; Abbrev
(with-eval-after-load 'abbrev
  (if (file-exists-p abbrev-file-name) (quietly-read-abbrev-file)))
(setq-default abbrev-mode t)

;;; End
(provide 'set-completions)
;;; set-completions.el ends here
