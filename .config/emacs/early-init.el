;;; -*- lexical-binding: t; -*-

; See: https://github.com/jwiegley/dot-emacs/blob/master/init.el
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

; Start up screens
(setq initial-major-mode 'fundamental-mode
      inhibit-splash-screen t)

; Don't make unnecessary frame resizes
(setq frame-inhibit-implied-resize t)
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)
