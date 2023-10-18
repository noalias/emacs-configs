;;; init.el --- Simple config.                       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <noalias@LAPTOP-G0RSVTIK>
;; Keywords: convenience, convenience, convenience

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
;;;;Vars
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t
      ring-bell-function 'ignore)
(setq-default cursor-type 'bar)

(setq auto-save-default nil   ; quietly save
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      auto-save-delete-trailing-whitespace t
      delete-auto-save-files t)

;;;; Keybindings
(global-unset-key (kbd "C-x C-o"))
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x C-d"))
(global-unset-key (kbd "C-x C-k"))
(global-unset-key (kbd "C-h C-f"))
(global-unset-key (kbd "C-x C-p"))
(global-unset-key (kbd "C-h C-a"))
(global-unset-key (kbd "M-i"))

(global-set-key [remap list-buffers] #'ibuffer)

;;;; Modes
(dolist (mode '(fido-vertical-mode
		electric-pair-mode))
  (add-hook 'after-init-hook mode))

;;;; Theme
;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)
;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(load-theme 'modus-vivendi)

;;;; Tail
(if-let* ((dir (concat (file-name-parent-directory user-emacs-directory)
                       "lib"))
          ((file-exists-p dir)))
    (dolist (dir (directory-files dir :full))
      (if (file-directory-p dir)
          (add-to-list 'load-path dir))))

(ignore-errors
  (load (expand-file-name "experiment.el" user-emacs-directory)
	    :noerror
	    :nomessage))

(provide 'init)
;;; init.el ends here
