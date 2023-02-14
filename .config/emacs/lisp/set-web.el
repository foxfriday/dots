;;; set-web.el --- Web browsing tools set up -*- lexical-binding: t; -*-

;;; Commentary:
;; Web browsing.
;; - eww
;; - webjump (bookmarks for external browser)

;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
                   (require 'shr))
(require 'evil)
(require 'defaults)

;;; eww
(unless (fboundp 'eww-open-in-new-buffer) (autoload 'eww "eww" "Follow" t))
(keymap-set evil-normal-state-map "g x" 'eww-open-in-new-buffer)

(with-eval-after-load 'eww
  (setq shr-max-width 90)
  (evil-define-key 'normal eww-mode-map
    ; go
    "gc" 'url-cookie-list
    "gb" 'eww-list-bookmarks
    "gh" 'eww-list-histories
    "go" 'eww-browse-with-external-browser
    "gr" 'eww-reload
    "gs" 'eww
    ; views
    "B"  'eww-add-bookmark
    "R"  'eww-readable
    "S"  'eww-view-source
    ; actions
    "C"  'eww-copy-page-url
    "O"  'org-eww-copy-for-org-mode
    ; navigate
    ">"  'eww-forward-url
    "<"  'eww-back-url
    "]]" 'shr-next-link
    "[[" 'shr-previous-link
    "q"  'kill-this-buffer)
  (evil-define-key 'normal eww-history-mode-map
    (kbd my-return) 'eww-history-browse
    "gr" 'revert-buffer
    "q"  'kill-this-buffer)
  (evil-define-key 'normal eww-bookmark-mode-map
    (kbd my-return) 'eww-bookmark-browse
    "gr" 'revert-buffer
    "dd" 'eww-bookmark-kill
    "C"  'eww-bookmark-yank
    "q"  'kill-this-buffer)
  (evil-define-key 'normal url-cookie-mode-map
    "gr" 'revert-buffer
    "dd" 'url-cookie-delete
    "u"  'url-cookie-undo
    "q"  'kill-this-buffer))
;;; webjump
(with-eval-after-load 'webjump
  (let ((f (concat user-emacs-directory "webjump-links.el")))
    (if (file-exists-p f) (load f))))
;;; End
(provide 'set-web)
;;; set-web.el ends here
