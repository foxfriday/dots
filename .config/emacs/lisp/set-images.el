;;; set-images.el --- Base Emacs set up -*- lexical-binding: t; -*-

;;; Commentary:
;; Image viewing utilities
;; - image-mode

;;; Code:
(require 'evil)

;;; Image Mode
(with-eval-after-load 'image-mode
  (evil-define-key 'normal image-mode-map
    "H" 'image-flip-horizontally
    "V" 'image-flip-vertically
    "R" 'image-rotate
    "<" 'image-previous-file
    ">" 'image-next-file
    "+" 'image-increase-size
    "-" 'image-decrease-size
    "l" 'image-scroll-left
    "h" 'image-scroll-right
    "j" 'image-scroll-up
    "k" 'image-scroll-down
    "q" 'image-kill-buffer))

;;; End
(provide 'set-images)
;;; set-images.el ends here
