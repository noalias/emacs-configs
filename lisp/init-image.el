;;; init-image.el --- Config for view and edit image.  -*- lexical-binding: t; -*-

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
(with-eval-after-load 'image-crop
  (add-to-list 'image-crop-crop-command "gm")
  (add-to-list 'image-crop-cut-command "gm")
  (add-to-list 'image-crop-resize-command "gm")
  (add-to-list 'image-crop-rotate-command "gm"))

(provide 'init-image)
;;; init-image.el ends here
