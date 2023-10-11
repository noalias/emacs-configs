;;; fd-dired.el --- Dired the output of `fd'.        -*- lexical-binding: t; -*-

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

;;; Code:
(require 'find-dired)

(defvar fd-dired-args nil
  "Last arguments given to `fd' by \\[fd-dired-dired]")
;; History of fd-args values entered in the minibuffer.

(defvar fd-dired-args-history nil)

(defconst fd-dired-fd-pre-args
  (concat " --color never"
          (if (eq system-type 'windows-nt)
              (concat " --path-separator "
                      (shell-quote-argument "/")))
          " "))

(defconst fd-dired-rg-pre-args
  (concat " --color never"
          " --files-with-matches"
          " -0"
          " --regexp"
          " "))

(defun fd-dired (dir args)
  "Run `fd' and go into Dired mode on a buffer of the output.
The default command run is
fd ARGS -l
."
  (interactive (list
                (read-directory-name "Run fd in directory: " nil "" t)
                (read-string "Run fd (with args): " fd-dired-args
                             (if fd-dired-args
                                 '(fd-dired-args-history . 1)
                               'fd-dired-args-history))))
  (setq fd-dired-args args
        args (concat "fd"
                     fd-dired-fd-pre-args
                     (unless (string= args "")
                       (concat args " "))
                     "-l"))
  (find-dired-with-command dir args))

(defun fd-dired-dwim (&optional args)
  "Run `fd' and go into Dired mode on a buffer of the output in `default-directory'."
  (interactive "P")
  (cond
   ((consp args)
    (call-interactively #'fd-dired))
   (t
    (fd-dired default-directory
              (read-string "Run fd (with args): " fd-dired-args
                           (if fd-dired-args
                               '(fd-dired-args-history . 1)
                             'fd-dired-args-history))))))

(defun fd-name-dired (dir pattern)
  "Search DIR recursively for files matching the globbing PATTERN.
and run Dired on those files.
PATTERN is a shell wildcard (not an Emacs regexp) and need not be quoted.
The default command run is
fd -g PATTERN -l
."
  (interactive
   "DFd-name (directory): \nsFd-name (filename wildcard): ")
  (fd-dired dir (concat "-g" " " (shell-quote-argument pattern))))

(defun fd-rg-dired (dir regexp)
  "Find files in DIR that contain matches for REGEXP and Dired on output.
The default command run is
fd -X rg -l0 --regexp REGEXP | xargs -0 ls
."
  (interactive "DFd-rg (directory): \nsFd-rg (rg regexp): ")
  (find-dired-with-command dir
                           (concat "fd"
                                   fd-dired-fd-pre-args
                                   "-X"
                                   " "
                                   "rg"
                                   fd-dired-rg-pre-args
                                   (shell-quote-argument regexp)
                                   " "
                                   "|"
                                   " "
                                   "xargs -0"
                                   " "
                                   "ls"
                                   " "
                                   (cdr find-ls-option-default-ls))))

(provide 'fd-dired)
;;; fd-dired.el ends here
