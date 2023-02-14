;;; set-ledger.el --- Accounting -*- lexical-binding: t; -*-

;;; Commentary:
;; Set up accounting.

;;; Code:
(require 'yasnippet)

(add-hook 'ledger-mode-hook 'yas-minor-mode)
(add-hook 'ledger-mode-hook 'outline-minor-mode)
(add-hook 'ledger-mode-hook 'ledger-flymake-enable)

(with-eval-after-load 'ledger-mode
  (setq-local cape-dict-file "~/Repos/ledger/data/dictionary.txt"
              completion-at-point-functions (list #'cape-dict))
  (require 'yasnippet)
  (let ((snips "~/Repos/ledger/snippets"))
    (when (file-directory-p snips)
      (add-to-list 'yas-snippet-dirs snips)
      (yas-reload-all))))

(provide 'set-ledger)
;;; set-ledger.el ends here
