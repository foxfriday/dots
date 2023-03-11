;;; set-ledger.el --- Accounting -*- lexical-binding: t; -*-

;;; Commentary:
;; Set up accounting.

;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
                   (add-to-list 'load-path (concat user-emacs-directory "pkgs/ledger"))
                   (require 'ledger-mode))

(require 'yasnippet)

(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
(add-hook 'ledger-mode-hook 'yas-minor-mode)
(add-hook 'ledger-mode-hook 'outline-minor-mode)
(add-hook 'ledger-mode-hook 'ledger-flymake-enable)

(with-eval-after-load 'ledger-mode
  (require 'ledger-flymake)
  (setq-local cape-dict-file "~/Documents/ledger/data/dictionary.txt"
              completion-at-point-functions (list #'cape-dict))
  (require 'yasnippet)
  (let ((snips "~/Documents/ledger/snippets"))
    (when (file-directory-p snips)
      (add-to-list 'yas-snippet-dirs snips)
      (yas-reload-all)))
  ; Reports
  (defun ledger-report-period ()
    "Prompt for a period.

    A period may be in any of the following formats:

    2022, 2022/10, 2022/10/1, 10/1, october, oct, this week, this
    day, this month, this quarter, this year, next week, or last
    week."
    (read-string "Period: "))

  (add-to-list 'ledger-report-format-specifiers '("period" . ledger-report-period))

  (setq ledger-reports
        '(("Balances" "%(binary) -f %(ledger-file) bal")
          ("AGI" "%(binary) -Y -f %(ledger-file) -p %(period) reg ^Income: ^Expenses:Tax ^Assets:Pre:")
          ("Account Running Bal." "%(binary) -f %(ledger-file) reg %(account)")
          ("Tag Running Bal." "%(binary) -f %(ledger-file) reg ^Expenses: and %%(tagname)")
          ("Payee Running Bal." "%(binary) -f %(ledger-file) reg @%(payee)")))
  ; Bindings
  (evil-define-key 'normal ledger-report-mode-map
    "ge" 'ledger-report-edit-report
    "gr" 'ledger-report-redo
    "gR" 'ledger-report-reverse-report
    "go" 'ledger-report-goto
    "gs" 'ledger-report-save
    "q"  'ledger-report-quit))

(provide 'set-ledger)
;;; set-ledger.el ends here
