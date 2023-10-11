;;; -*- lexical-binding: t -*-
(use-package emacs
  :init
  (setq auto-save-silent t   ; quietly save
        make-backup-files nil
        auto-save-default nil
        create-lockfiles nil
        auto-save-delete-trailing-whitespace t
        delete-auto-save-files t)
  (defvar-keymap file:command-map
    "l" #'recentf
    "r" #'rename-file
    "c" #'copy-file
    "k" #'delete-file)
  (fset 'file:command-map file:command-map)
  :hook (after-init-hook . recentf-mode)
  :bind
  ("C-c f" . file:command-map)
  :custom
  (recentf-exclude `(,no-littering-var-directory
                     ,no-littering-etc-directory
                     "Scoop"))
  :config
  ;; Active 'find file at point'.
  (ffap-bindings))

(provide 'init-file)
;;; init-file.el ends here
