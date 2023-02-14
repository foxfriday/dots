;;; set-prose.el --- Writing tools set up -*- lexical-binding: t; -*-

;;; Commentary:
;; Some common configurations for prose writing.

;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
                   (require 'dictionary)
                   (require 'ispell))
(require 'defaults)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'prettify-symbols-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

(with-eval-after-load 'ispell
  (let ((d "~/.config/dict/personal.dic"))
    (if (file-exists-p d)
        (setq ispell-personal-dictionary d)))
  (setq ispell-dictionary "american")
  (add-to-list 'ispell-skip-region-alist '("^#+begin" . "^#+end_src")))

(with-eval-after-load 'dictionary
  (setq dictionary-server (if is-mac "dict.org" "localhost"))
  (setq dictionary-use-single-buffer t))
;;; End
(provide 'set-prose)
;;; set-prose.el ends here
