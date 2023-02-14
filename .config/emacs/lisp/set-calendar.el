;;; set-calendar.el --- Calendar, Diary, Etc set up -*- lexical-binding: t; -*-
;;; Code:
(eval-when-compile (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
                   (require 'appt)
                   (require 'calendar)
                   (require 'diary-lib)
                   (require 'holidays)
                   (require 'solar)
                   (require 'time))

(require 'defaults)
(require 'evil)

;;; World Clock
(with-eval-after-load 'time
  (evil-define-key 'normal world-clock-mode-map
    "q" 'kill-this-buffer)
  (setq world-clock-list '(("America/Los_Angeles" "Los Angeles")
                           ("America/Chicago" "Chicago")
                           ("America/New_York" "New York")
                           ("Europe/London" "London"))))
;;; Calendar
(setq calendar-date-style 'iso  ; Set before loading
      diary-file my-diary)

(with-eval-after-load 'calendar
  (setq calendar-mode-line-format nil
        calendar-latitude 33.37
        calendar-longitude -117.53
        calendar-holidays (append holiday-general-holidays
                                  holiday-christian-holidays
                                  holiday-solar-holidays))
  (evil-define-key 'normal calendar-mode-map
    ; navigation
    "0"  'calendar-beginning-of-week
    "$"  'calendar-end-of-week
    "[y" 'calendar-backward-year
    "]y" 'calendar-forward-year
    "[[" 'calendar-backward-month
    "]]" 'calendar-forward-month
    "<"  'calendar-scroll-right
    ">"  'calendar-scroll-left
    "."  'calendar-goto-today
    "gd" 'calendar-goto-date
    ; marks
    "u"  'calendar-unmark
    "x"  'calendar-mark-holidays
    "m"  'diary-mark-entries
    ; show
    "a"  'org-calendar-goto-agenda
    "d"  'diary-view-entries
    "h"  'calendar-cursor-holidays
    "gs" 'calendar-sunrise-sunset
    "gh" 'calendar-list-holidays
    ; common
    "gr" 'calendar-redraw
    "q"  'calendar-exit))

;;; Diary
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

(with-eval-after-load 'diary-lib
  (require 'date-functions)
  (defvar personal-anniversary-face '(:foreground "red")
    "Face used for anniversaries in calendar mode.")
  (defvar personal-expiration-face '(:foreground "pink")
    "Face used for expiration events in calendar mode.")
  (setq diary-entry-marker '(:foreground "green")
        diary-show-holidays-flag t)
  (appt-activate 1))

(with-eval-after-load 'appt
  (setq appt-display-diary nil ;; don't display on first load
        appt-display-interval 5
        appt-message-warning-time 30))
;;; End
(provide 'set-calendar)
