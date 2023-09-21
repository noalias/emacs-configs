;;;  -*- lexical-binding: t -*-
;;; Global requires
(require 'image-utility)

;;; Global Setting
(setq delete-by-moving-to-trash t)       ; Deleting files go to OS's trash folder

;;; dired
(use-package dired
  :init
  (defvar dired:externally-file-ext `("pdf"
                                      ,(rx bos
                                           (or (seq "do" (or ?c ?t) (? ?x))
                                               "ppt")
                                           eos)
                                      "dwg"
                                      "dxf"
                                      "DXF"
                                      "xlsx"))
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
        ("/ p" . dired:convert-image-to-pdf)
        ("/ i" . dired:convert-pdf-to-image)
        ("/ m" . dired:merge-pdf-files))
  :custom
  (dired-hide-details-hide-symlink-targets nil)
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group"
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        ;;        dired-kill-when-opening-new-dired-buffer t
        )
  
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  (defun dired:find-file-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (let ((open (lambda (file)
                  (if (and (eq system-type 'windows-nt)
                           (fboundp 'w32-shell-execute))
                      (w32-shell-execute "open" file)
                    (call-process (pcase system-type
                                    ('darwin "open")
                                    ('cygwin "cygstart")
                                    (_ "xdg-open"))
                                  nil 0 nil
                                  (expand-file-name file)))
                  (message "Opened \"%s\" successfully." file)))
          (file-ext-matched (lambda (file)
                              (seq-some
                               (lambda (ext)
                                 (string-match-p ext (file-name-extension file)))
                               dired:externally-file-ext))))
      (dired-map-over-marks
       (let ((file (dired-get-file-for-visit)))
         (if (or
              (file-directory-p file)
              (funcall file-ext-matched file))
             (funcall open file)))
       arg)))
  
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
