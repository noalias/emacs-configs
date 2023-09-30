;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-
(setq load-prefer-newer t)
(setq byte-compile-warnings nil)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
	(expand-file-name  "var/eln-cache/" user-emacs-directory))))

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "lib/compat" dir))
  (add-to-list 'load-path (expand-file-name "lib/auto-compile" dir)))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; packages config
(progn ;;`Packages'
  (setq package-enable-at-startup nil
        package-archives
        (list (cons "gnu" "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
              (cons "melpa" "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
              (cons "nognu" "http://mirrors.tuna.tsinghua.edu.cn/elpa/nognu/"))))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)
;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; 去掉标题行
(when (eq system-type 'gnu/linux)
  (push '(undecorated . t) default-frame-alist))
(push '(fullscreen . maximized) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; early-init.el ends here
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
