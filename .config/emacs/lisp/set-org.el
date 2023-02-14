;;; set-org.el --- My org-mode set up -*- lexical-binding: t; -*-

;;; Commentary:
;; Set up org mode, mostly agenda, and capture.

;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/")))
(require 'evil)
(require 'oc-bibtex)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'org-fold)
(require 'org-refile)
(require 'defaults)

;;; Citations
(setq org-cite-global-bibliography my-bibliography
      org-cite-export-processors '((beamer bibtex) (latex bibtex) (t basic)))

;;; Agenda
(evil-set-initial-state 'org-agenda-mode 'normal)

(if my-agenda (setq org-directory my-agenda
                    org-agenda-files my-agenda))

(setq org-agenda-show-future-repeats 'next
      org-agenda-compact-blocks t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t
      org-agenda-restore-windows-after-quit t
      org-agenda-include-diary t
      org-stuck-projects '("+@project" ("TODO") nil "")
      org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 60 :score 0))

;;; To Dos
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)"))
      org-todo-keyword-faces '(("CANCELED" . "orange") ("WAIT" . "green")))

(defun log-todo-state-properties ()
  "Add time properties when needed."
  (let* ((state (org-get-todo-state)))
    (when (and state (not (org-entry-get nil "CAPTURED")))
      (org-set-property "CAPTURED" (format-time-string "[%Y-%m-%d]")))
    (cond ((and (string-equal state "WAIT")
                (not (org-entry-get nil "WAIT_DATE")))
           (org-entry-put nil "WAIT_DATE" (format-time-string "[%Y-%m-%d %H:%M]")))
          ((and (string-equal state "CANCELED")
                (not (org-entry-get nil "FINISHED")))
           (org-entry-put nil "FINISHED" (format-time-string "[%Y-%m-%d %H:%M]"))
           (org-entry-put nil "CAUSE" (read-string "Cause: ")))
          ((and (string-equal state "DONE")
                (not (org-entry-get nil "FINISHED")))
           (org-entry-put nil "FINISHED" (format-time-string "[%Y-%m-%d %H:%M]"))))))
(add-hook 'org-capture-before-finalize-hook 'log-todo-state-properties)
(add-hook 'org-after-refile-insert-hook 'log-todo-state-properties)
(add-hook 'org-after-todo-state-change-hook 'log-todo-state-properties)

;;; Tags
(setq org-tag-alist '((:startgroup . nil)
                      ("@cat" . ?C)
                      ("@project" . ?P)
                      ("@notes" . ?N)
                      (:endgroup . nil))
      org-tags-exclude-from-inheritance '("@project" "@cat"))

;;; Capture
; evil-set-initial-state only work with major modes
(add-hook 'org-capture-mode-hook 'evil-insert-state)
(setq org-capture-templates
      (append
       `(("t" "To do" entry (file "agenda.org")
          "** TODO %^{ACTION} %?\n")
         ("s" "Start" entry (file "agenda.org")
          "** TODO %^{ACTION} %?\n" :clock-in 1 :clock-keep 1)
         ("i" "Idea" entry (file "ideas.org")
          "* MAYBE %^{IDEA}\n %i%?")
         ("n" "Notes")
         ("nc" "Clock" plain (clock) "[%Y-%m-%d %H:%M]: %?" :empty-line 1)
         ("nd" "Diary" entry (file+olp+datetree ,diary-org)
          "* %<%H:%M> %^{PROMPT|Day Summary} %^g\n %?" :tree-type week)
         ("nm" "Meeting" entry (file+olp+datetree ,diary-org)
          "* %<%H:%M> %^{PROMPT} %(org-set-tags \":@mtg:\") %^{attendees}p \n%?" :tree-type week))
       org-capture-templates))

(setq org-refile-targets '((org-agenda-files :tag . "@cat")
                           (org-agenda-files :tag . "@project"))
      org-capture-bookmark nil)
;;; Links
(defun org-link-track-ups (path arg)
  "Track a ups package where PATH is the tracking number and ARG unused."
  (browse-url (url-encode-url (concat "https://www.ups.com/track?loc=en_US&tracknum=" path)) arg))

(defun org-link-track-usps (path arg)
  "Track a usps package where PATH is the tracking number and ARG unused."
  (browse-url (url-encode-url (concat "https://tools.usps.com/go/TrackConfirmAction.action?tLabels=" path)) arg))

(defun org-link-track-fedex (path arg)
  "Track a fedex package where PATH is the tracking number and ARG unused."
  (browse-url (url-encode-url (concat "https://www.fedex.com/fedextrack/?trknbr=" path)) arg))

(defun org-link-track-dhl (path arg)
  "Track a DHL package where PATH is the tracking number and ARG unused."
  (browse-url (url-encode-url (concat "https://www.dhl.com/us-en/home/tracking/tracking-express.html?submit=1&tracking-id=" path)) arg))

(org-link-set-parameters "ups" :follow 'org-link-track-ups :face '(:foreground "green" :underline t))
(org-link-set-parameters "usps" :follow 'org-link-track-usps :face '(:foreground "green" :underline t))
(org-link-set-parameters "fedex" :follow 'org-link-track-fedex :face '(:foreground "green" :underline t))
(org-link-set-parameters "dhl" :follow 'org-link-track-dhl :face '(:foreground "green" :underline t))

;;; Theme
(setq org-startup-folded 'content
      org-edit-src-content-indentation 0
      org-startup-with-inline-images t
      org-image-actual-width '(300)
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-fold-catch-invisible-edits 'show
      org-fontify-quote-and-verse-blocks t
      org-ellipsis "⤵")

(set-face-attribute 'org-block-begin-line nil :foreground "black" :background "SeaGreen")
;;; Babel
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                         (shell . t)
                                                         (sqlite . t)
                                                         (dot . t) ; graphviz
                                                         (python . t)))

;;; Other
(setq org-columns-default-format "%4TODO(Status) %20ITEM(Task) %Effort(EFFORT) %CAPTURED(START) %WAIT_DATE(WAIT)"
      org-global-properties '(("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 8:00"))
      org-clock-out-remove-zero-time-clocks t
      org-clock-report-include-clocking-task t
      org-archive-location "archive.org::* Archived from %s")

;;; Bindings
(keymap-set org-mode-map "C-c [" 'org-cite-insert)

(evil-define-key 'normal org-mode-map
  "zA" 'org-cycle
  "ge" 'org-edit-special
  "go" 'org-open-at-point)
(evil-define-key nil my-leader-map
  "L" 'org-insert-link
  "E" 'org-set-effort
  "T" 'org-set-tags-command
  "P" 'org-set-property
  "A" 'org-archive-subtree)
(evil-define-key 'normal org-agenda-mode-map
  "e"  'org-agenda-set-effort
  "t"  'org-agenda-set-tags
  "p"  'org-agenda-set-property
  "a"  'org-agenda-archive
  "n"  'org-agenda-add-note
  "f"  'org-agenda-follow-mode
  "se" 'org-agenda-filter-by-effort
  "st" 'org-agenda-filter-by-tag
  "sc" 'org-agenda-filter-by-category
  "ci" 'org-agenda-clock-in
  "co" 'org-agenda-clock-out
  "cr" 'org-agenda-clockreport-mode
  "gr" 'org-agenda-redo-all
  "+"  'org-agenda-priority-up
  "-"  'org-agenda-priority-down
  "/"  'org-agenda-filter
  "<"  'org-agenda-earlier
  ">"  'org-agenda-later
  "vd" 'org-agenda-day-view
  "vw" 'org-agenda-week-view
  "vf" 'org-agenda-fortnight-view
  "vm" 'org-agenda-month-view
  "C"  'org-agenda-columns
  "m"  'org-agenda-bulk-mark
  "u"  'org-agenda-bulk-unmark
  "a"  'org-agenda-bulk-action
  (kbd my-return) 'org-agenda-goto
  "."  'org-agenda-goto-today
  "gs" 'org-agenda-sunrise-sunset
  "gd" 'org-agenda-goto-date
  "q"  'org-agenda-Quit)

(provide 'set-org)
;;; set-org.el ends here
