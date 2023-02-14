(require 'yasnippet)
(defvar yas-text)

(defun yas-choose-image ()
  (unless (or yas-moving-away-p yas-modified-p)
    (let* ((choices (directory-files-recursively "." ".png$\\|.gif$\\|.tiff$\\|.jpeg$\\|.pnm$")))
      (completing-read "File:" choices))))
