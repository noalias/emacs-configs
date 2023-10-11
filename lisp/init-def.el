;;; init-def.el --- Variables,functions.             -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <noalias@LAPTOP-G0RSVTIK>
;; Keywords: tools

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

;;; Code:
;;; VARS
(defconst def:win-p (eq system-type 'windows-nt))
(defconst def:linux-p (eq system-type 'gnu/linux))

;;; FUNCTIONS
(defun def:expand-scoop-bin-file-name (app &optional subdir)
  (or subdir (setq subdir ""))
  (when-let (dir (getenv "SCOOP"))
    (concat dir "\\apps\\" app "\\current\\" subdir)))

(defun def:find-file-externally (file)
  "INSTALL `open' \"cargo install open\"."
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" file)
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil
                  (expand-file-name file)))
  (message "Opened \"%s\" successfully." file))



(provide 'init-def)
;;; init-def.el ends here
