;;; set-theme.el --- GUI Theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Set `modus-vivendi` as the theme along with fonts. Only useful on GUI.

;;; Code:

(load-theme 'modus-vivendi t)

(set-face-attribute 'fringe nil :background "black")
(set-face-attribute 'line-number nil :background "black")

(let* ((font-fp (cond ((find-font (font-spec :name "JetBrains Mono")) "Jetbrains Mono")
                      ((find-font (font-spec :name "DejaVu Sans Mono")) "DejaVu Sans Mono")
                      (t nil)))
       (font-vp (cond ((find-font (font-spec :name "Menlo")) "Menlo")
                      ((find-font (font-spec :name "DejaVu Sans")) "DejaVu Sans")
                      (t nil)))
       (height (if (> (x-display-mm-height) 300) (if (eq system-type 'darwin) 200 150) 130)))
  (when font-fp
    (set-face-attribute 'default nil :family font-fp :height height :weight 'normal)
    (set-face-attribute 'fixed-pitch nil :family font-fp :height height :weight 'light))
  (when font-vp
    (set-face-attribute 'variable-pitch nil :family font-vp :height height :weight 'normal)))

;;; End
(provide 'set-theme)
;;; set-theme.el ends here
