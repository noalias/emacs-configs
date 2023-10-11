;;; denote-collection.el --- Use `denote' to collecte files.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <noalias@LAPTOP-G0RSVTIK>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;; Code:
(require 'denote)
(require 'dired)

(defvar denote-collection-directory
  (expand-file-name "~/Reference/")
  "A directory to store files.")

(defun denote-collection-new-file (file keywords &optional tittle date)
  (let ((id (denote-retrieve-or-create-file-identifier file date :unique))
        (file-type (denote-filetype-heuristics file))
        (signature (denote-retrieve-filename-signature file))
        (extension (file-name-extension file t)))
    (or tittle
        (setq tittle (denote--retrieve-title-or-filename file file-type)))
    (denote-format-file-name
     (file-name-as-directory denote-collection-directory)
     id
     keywords
     (denote-sluggify tittle)
     extension
     signature)))

(defun denote-collection-get-file (file keywords &optional date)
  (interactive
   (let* ((file (read-file-name "Collect Denote-style file: "
                                nil nil t))
          (file-type (denote-filetype-heuristics file)))
     (list
      file
      (denote-keywords-prompt)
      current-prefix-arg)))
  (let ((tittle))
    (if-let ((name (denote-collection-new-file file keywords tittle date)))
        (unless (file-exists-p name)
          (rename-file file name)))))

(defalias 'denote-collect-file 'denote-collection-get-file)

(defun denote-collection-dired-get-file (&optional arg)
  (interactive "P" 'dired-mode)
  (let ((keywords (denote-keywords-prompt)))
    (dired-map-over-marks
     (let ((file (dired-get-file-for-visit)))
       (if-let ((name (denote-collection-get-file file keywords)))
           (unless (and (file-directory-p file)
                        (file-exists-p name))
             (dired-rename-file file name))))
     arg))
  (revert-buffer))

(defalias 'denote-dired-collect-file 'denote-collection-dired-get-file)

(provide 'denote-collecte)
;;; denote-collecte.el ends here
