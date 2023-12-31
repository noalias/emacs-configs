;;; -*- lexical-binding: t -*-
;; Config dir
(use-package no-littering)
;; Package db
(use-package epkg
  :init
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))

(use-package emacs
  :demand t
  :hook (after-init-hook . savehist-mode)
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  ;;; (server-auth-dir (no-littering-expand-var-file-name "server"))
  :config
  (when (file-exists-p custom-file)
    (load custom-file))

  (progn ; `Encoding'
    ;; (set-language-environment               "UTF-8")     ;; System default coding
    ;; (prefer-coding-system                   'utf-8)      ;; prefer
    ;; (set-buffer-file-coding-system          'utf-8-unix) ;;
    ;; (set-charset-priority                   'unicode)    ;;
    ;; (set-clipboard-coding-system            'utf-8-unix) ;; clipboard
    ;; (set-default-coding-systems             'utf-8)      ;; buffer/file: 打开文件时的默认编码
    ;; (set-file-name-coding-system            'utf-8-unix) ;; unix/linux/macos
    ;; (set-keyboard-coding-system             'utf-8-unix) ;; keyboard
    ;; (set-next-selection-coding-system       'utf-8-unix) ;; selection
    ;; (set-selection-coding-system            'utf-8)      ;; selection
    ;; (set-terminal-coding-system             'utf-8-unix) ;; terminal
    ;; (setq coding-system-for-read            'utf-8)      ;;
    ;; (setq default-buffer-file-coding-system 'utf-8)      ;;
    ;; (setq locale-coding-system              'utf-8)      ;; local
    ;; UTF-8 as the default coding system
    (set-charset-priority 'unicode)
    (prefer-coding-system 'utf-8)
    ;; (setq locale-coding-system 'utf-8)
    (setq system-time-locale "C")
    (when def:win-p
      ;; (set-file-name-coding-system 'chinese-gbk)
      (setq default-process-coding-system '(utf-8 . gbk))
      (modify-coding-system-alist 'process
                                  (rx (or (and (or ?c ?C)
                                               (or ?m ?M)
                                               (or ?d ?D)
                                               (or ?p ?P)
                                               (or ?r ?R)
                                               (or ?o ?O)
                                               (or ?x ?X)
                                               (or ?y ?Y))
                                          "gm" "magick" "7z" "es" "fd" "rg" "xargs" "ls"))
                                  '(utf-8-auto . chinese-gbk-dos))))
  (progn ; `path'
    (when def:win-p
      (let ((path (def:expand-scoop-bin-file-name "git-with-openssh" "usr\\bin")))
        (add-to-list 'exec-path path)
        (setenv "PATH" (concat path ";" (getenv "PATH"))))))

  (progn ; `unset-keys'
    (global-unset-key (kbd "C-x C-o"))
    (global-unset-key (kbd "C-x f"))
    (global-unset-key (kbd "C-x C-d"))
    (global-unset-key (kbd "C-x C-k"))
    (global-unset-key (kbd "C-h C-f"))
    (global-unset-key (kbd "C-x C-p"))
    (global-unset-key (kbd "C-h C-a")))

  (progn ; `misc'
    (fset 'yes-or-no-p 'y-or-n-p)
    (setq visible-bell t
          ring-bell-function 'ignore)
    (setq-default cursor-type 'bar))

  (server-mode)
  )

(provide 'init-base)
;;; init-base.el ends here
