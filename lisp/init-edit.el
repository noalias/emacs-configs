;;;  -*- lexical-binding: t -*-
(use-package emacs
  :init
  :config
  (add-hook 'after-init-hook #'global-hl-line-mode)
  (add-hook 'after-init-hook #'electric-pair-mode)
  (add-hook 'after-init-hook #'auto-insert-mode)
  (setq auto-insert-directory (expand-file-name "template" user-emacs-directory))
  (setq-default fill-column 80
                tab-width 4
                indent-tabs-mode nil))

(use-package view-mode
  :bind
  ("C-;" . view-mode))

(use-package rect
  :bind
  (
   :map rectangle-mark-mode-map
   ("i" . string-insert-rectangle)
   ("M-w" . copy-rectangle-as-kill)
   ("C-w" . kill-rectangle)
   ("C-d" . delete-rectangle))
  )

(use-package paren
  :hook (after-init-hook . show-paren-mode)
  :custom
  (show-paren-context-when-offscreen 'overlay)
  (show-paren-when-point-inside-paren t))

(use-package avy
  :bind
  (("C-'" . avy-goto-char)
   ("M-'" . avy-goto-char-2)
   ("C-," . avy-goto-word-crt-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ("M-g g" . avy-goto-line))
  :config
  (setq avy-background t)
  (defun avy-goto-word-crt-line ()
    "Jump to a word start on the current line only."
    (interactive)
    (avy-with avy-goto-word-0
      (avy-goto-word-0 nil (line-beginning-position) (line-end-position)))))

(use-package aggressive-indent
  :hook (after-init-hook . global-aggressive-indent-mode))

(provide 'init-edit)
