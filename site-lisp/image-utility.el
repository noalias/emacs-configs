;;; -*- lexical-binding: t -*-

;;;; image-converter
(defvar mupdf-tool (or (executable-find "mutool")
                       (user-error "Can not find `mutool'.")))

(defun image-utility--convert (file ext &optional output)
  "Convert image file."
  (if (and output
           (not (string= (file-name-extension output)
                         ext)))
      (user-error "The extension of output MUST equal EXT."))
  (unless output
    (setq output (file-name-sans-extension file)))
  (unless (featurep 'image-converter)
    (require 'image-converter))
  (let ((image-convert-to-format ext))
    (unless (file-name-extension output)
      (setq output (concat output "." ext)))
    (with-temp-file output
      (insert (image-convert file))
      output)))

(defun image-utility--merge-files (output &rest inputs)
  (unless (file-name-extension output)
    (setq output (concat output ".pdf")))
  (apply #'process-lines
         mupdf-tool
         "merge"
         (format "-o%s" (expand-file-name output))
         (mapcar #'expand-file-name inputs)))

(defun image-utility--read-text (pdfile)
  (process-lines mupdf-tool "draw" "-Ftext" (expand-file-name pdfile)))

(provide 'image-utility)
