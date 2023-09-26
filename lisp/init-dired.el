;;;  -*- lexical-binding: t -*-
;;; Global requires
(require 'image-utility)

;;; Global Setting
(setq delete-by-moving-to-trash t)       ; Deleting files go to OS's trash folder

;;; dired
(use-package dired
  :init
  (defvar dired:externally-file-ext-regex (rx bos
                                             (or (and "do" (or ?c ?t) (? ?x))
                                                 (and "ppt" (? ?x))
                                                 "pdf"
                                                 "mp4"
                                                 "dwg"
                                                 "dxf"
                                                 "DXF"
                                                 "xlsx"
                                                 )
                                             eos)
    :bind
    (:map dired-mode-map
          ("," . dired-create-empty-file)
          ;; 折叠子目录
          ("TAB" . dired-hide-subdir)
          ("C-k" . dired-kill-subdir)
          ("M-p" . dired-prev-subdir)
          ("M-n" . dired-next-subdir)
          ;; `f' 进入目录或文件
          ;; `b' 返回上级目录
          ("b" . dired-up-directory)
          ("e" . dired:find-file-externally)
          ("E" . dired-toggle-read-only)
          ("/ u" . dired-upcase)
          ("/ l" . dired-downcase)
          ("/ d" . dired-flag-files-regexp)
          ("/ g" . dired-mark-files-containing-regexp)
          ("/ m" . dired-mark-files-regexp)
          ("/ r" . dired-do-rename-regexp)
          ("/ C" . dired-do-copy-regexp)
          ("/ H" . dired-do-hardlink-regexp)
          ("/ R" . dired-do-rename-regexp)
          ("/ S" . dired-do-symlink-regexp)
          ("/ Y" . dired-do-relsymlink-regexp)
          ("/ &" . dired-flag-garbage-files)
          ("SPC" . dired:utilities))
    :custom
    (dired-hide-details-hide-symlink-targets nil)
    :config
    (setq dired-listing-switches
          "-l --almost-all --human-readable --group-directories-first --no-group"
          dired-recursive-deletes 'always
          dired-recursive-copies 'always
          ;;        dired-kill-when-opening-new-dired-buffer t
          )

    (define-keymap 
      :prefix 'dired:utilities
      "p" #'dired:convert-image-to-pdf
      "i" #'dired:convert-pdf-to-image
      "m" #'dired:merge-pdf-files)

    (add-hook 'dired-mode-hook #'dired-hide-details-mode)

    (defun dired:find-file-externally (&optional arg)
      "Open marked or current file in operating system's default application."
      (interactive "P")
      (dired-map-over-marks
       (let ((file (dired-get-file-for-visit)))
         (if (or (file-directory-p file)
                 (string-match-p dired:externally-file-ext-regex
                                 (file-name-extension file)))
             (def:find-file-externally file)))
       arg))

  (defun dired:merge-pdf-files (name)
    "将 `image' 文件及 `pdf' 合并为一个 `pdf' 文件"
    (interactive "sOutput file name: ")
    (let ((files (dired-get-marked-files))
          (default-directory (dired-current-directory)))
      (if (length< files 2)
          (user-error "Less files to merge"))
      (apply #'image-utility--merge-files name
             (mapcar (lambda (file)
                       (pcase (file-name-extension file)
                         ((or "png" "pdf") file)
                         (_ (image-utility--convert file))))
                     files))))

  (defun dired:convert-image-to-pdf (&optional arg)
    "将 `image' 文件转化为 pdf 文件"
    (interactive "P")
    (let ((default-directory (dired-current-directory)))
      (dired-map-over-marks
       (image-utility--convert (dired-get-filename) "pdf")
       arg)))

  (defun dired:convert-pdf-to-image (&optional arg)
    "将 `pdf' 文件转化为 `image' 文件"
    (interactive "P")
    (let ((default-directory (dired-current-directory)))
      (dired-map-over-marks
       (image-utility--convert (dired-get-filename) "png")
       arg)))
  )

(use-package dired-x
  :hook (dired-mode-hook . dired-omit-mode)
  :config
  (setq dired-omit-files
        (rx bos (or (seq "desktop.ini")
                    (seq ?~ (? ?$) (* (or alnum (category chinese-two-byte))) (? ".tmp"))
                    eos))))

(use-package dired-aux
  :defer
  :custom
  (dired-compress-directory-default-suffix ".7z")
  :config
  (add-to-list 'dired-compress-files-alist '("\\.7z\\'" . "7z a %o -r %i"))
  (add-to-list 'dired-compress-file-suffixes `(,(rx ?. (or "7z" "zip" "rar" "gz") eos)
                                               ""
                                               "7z x -aoa -o%o %i")))

(use-package diredfl
  :hook dired-mode-hook
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package nerd-icons-dired
  :hook dired-mode-hook)


(provide 'init-dired)
;;; init-dired.el ends here
