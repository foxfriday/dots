;;; set-code.el --- Programming tools set up -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(eval-when-compile (require 'eglot)
                   (require 'python))

(require 'evil)
(require 'yasnippet)
(require 'extra)

;;; Flymake
; check with flymake-running-backends
(add-hook 'LaTeX-mode-hook 'flymake-mode) ; requires chktex
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(add-hook 'python-base-mode-hook 'flymake-mode) ; requires ruff

(setq-default sh-shellcheck-arguments (list "-x")) ; follow sourced libraries
(add-hook 'sh-base-mode-hook 'flymake-mode)

(with-eval-after-load 'flymake
  (evil-define-key 'normal flymake-diagnostics-buffer-mode-map
    (kbd my-return) 'flymake-goto-diagnostic
    "s" 'flymake-show-diagnostic
    "q" 'quit-window)
  (evil-define-key 'normal flymake-mode-map
    "gl" 'flymake-show-buffer-diagnostics
    "gL" 'flymake-show-project-diagnostics
    "]e" 'flymake-goto-next-error
    "[e" 'flymake-goto-prev-error))

;;; Eglot
(defun maybe-eglot ()
  "Initiate if '.lsp' file if found at the root directory."
  (if (locate-dominating-file default-directory ".lsp")
      (eglot-ensure)))

(defvar ltex-ls (eval-when-compile (executable-find "ltex-ls"))
  "Path to ltex language server.")

(when ltex-ls
  (add-hook 'LaTeX-mode-hook 'maybe-eglot)
  (add-hook 'org-mode-hook 'maybe-eglot))

(add-hook 'python-base-mode-hook 'maybe-eglot)

; Done added AFTER eglot is initialized (verify with flymake-running-backends)
(add-hook 'eglot-managed-mode-hook
          (lambda () (cond ((derived-mode-p 'tex-mode)
                            (require 'latex-flymake)
                            (add-hook 'flymake-diagnostic-functions 'LaTeX-flymake nil t))
                           (t nil))))

(with-eval-after-load 'eglot
  (setq eldoc-echo-area-use-multiline-p nil
        ; cat /proc/sys/fs/pipe-max-size
        read-process-output-max (* 1024 1024))
  (when ltex-ls
    (add-to-list 'eglot-server-programs `(latex-mode . (,ltex-ls)))
    (add-to-list 'eglot-server-programs `(org-mode . (,ltex-ls))))
  (evil-define-key 'normal eglot-mode-map
    "gR" 'eglot-rename
    "ga" 'eglot-code-actions
    "gf" 'eglot-format-buffer
    "gr" 'xref-find-references
    "gd" 'xref-find-definitions
    "gh" 'eldoc))

;;; Project
(with-eval-after-load 'project
  (defun extra-project-finder (dir)
    "Use `project.el` on DIR with not git managed projects."
    (let ((proj (and (setq dir (locate-dominating-file dir ".project"))
                     (expand-file-name dir))))
      (and proj (cons 'transient (file-name-directory proj)))))
  (add-to-list 'project-find-functions 'extra-project-finder))

;;;; CSS
(with-eval-after-load 'css
  (evil-define-key 'normal css-ts-mode-map
    "gH" 'css-lookup-symbol
    "gc" 'list-colors-display
    "cc" 'css-cycle-color-format))

;;;; Python
(defun extra-jupyter-launch ()
  "Start an async Jupyter Lab session."
  (interactive)
  (let* ((default-directory (read-directory-name "Directory:" default-directory))
         (log-buffer (get-buffer-create "*Jupyter*")))
    (async-shell-command "jupyter-notebook" log-buffer log-buffer)))

(with-eval-after-load 'python
  (keymap-set python-ts-mode-map "M-q" (lambda () (extra-format-buffer "docformatter")))
  (keymap-set python-ts-mode-map "M-b" (lambda () (extra-format-buffer "black")))
  (setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-")
        python-shell-dedicated 'project
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --pprint"))

(provide 'set-code)
;;; set-code.el ends here
