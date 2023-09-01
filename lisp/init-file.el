;;; -*- lexical-binding: t -*-
(use-package emacs
  :init
  (setq auto-save-silent t   ; quietly save
        make-backup-files nil
        auto-save-default nil
        create-lockfiles nil
        auto-save-delete-trailing-whitespace t
        delete-auto-save-files t)
  :hook (after-init-hook . recentf-mode)
  :bind
  ("C-x f" . find-file)
  :custom
  (recentf-exclude `(,no-littering-var-directory
                     ,no-littering-etc-directory
                     ,(rx bos ?.
                          (or (seq "do" (or ?c ?t) (? ?x))
                              "ppt")
                          eos)))
  :config
  (defconst clip-command (executable-find "/mnt/c/WINDOWS/system32/clip.exe"))

  (defun wsl-cut-region-to-clipboard (start end)
    (interactive "r")
    (if (null clip-command)
        (user-error "Cannot find clip.exe.")
      (call-process-region start end clip-command nil 0)
      (kill-region start end)))

  (defun wsl-copy-region-to-clipboard (start end)
    (interactive "r")
    (if (null clip-command)
        (user-error "Cannot find clip.exe.")
      (call-process-region start end clip-command nil 0)
      (kill-ring-save start end)))
  )

(provide 'init-file)
;;; init-file.el ends here
