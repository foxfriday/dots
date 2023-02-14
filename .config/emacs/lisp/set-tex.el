;;; set-tex.el --- LaTeX tools set up -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure LaTeX

;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
                   (require 'bibtex)
                   (require 'reftex)
                   (require 'tex))

(require 'defaults)
(require 'yasnippet)

;;; LaTeX
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(with-eval-after-load 'tex
  (add-hook 'LaTeX-mode-hook (lambda () (turn-on-reftex) (TeX-fold-mode 1) (yas-minor-mode)))
  (setq TeX-engine 'luatex
        TeX-auto-save t
        TeX-parse-self t
        TeX-electric-math (cons "\\(" "\\)")
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t
        TeX-master nil
        reftex-plug-into-AUCTeX t)
  (if is-mac
      (add-to-list 'TeX-view-program-selection '(output-pdf "Skim"))
    (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))))

(with-eval-after-load 'bibtex
  (defun isbn-to-bibtex (isbn)
    "Retrieve `bibtex` entry from ISBN."
    (interactive "sISBN: ")
    (let* ((url (format "https://www.ebook.de/de/tools/isbn2bibtex?isbn=%s" isbn))
	   (entry))
      (with-current-buffer (url-retrieve-synchronously url t t)
        (goto-char (point-min))
        (when (re-search-forward "@[a-zA-Z]+{.+\\(\n\s+[^\n]+\\)+}$" nil t)
	  (setq entry (match-string 0))))
      (if entry
          (insert (concat entry "\n}"))
        (error "No valid response"))))

  (setq bibtex-align-at-equal-sign t
        bibtex-entry-format t))

(setq-default reftex-default-bibliography my-bibliography)

(provide 'set-tex)
;;; set-tex.el ends here
