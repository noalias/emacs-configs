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

(use-package view
  :bind
  (("C-;" . view-mode)
   ([remap read-only-mode] . view-mode)
   ([remap find-file-read-only] . view-file)
   ([remap find-file-read-only-other-frame] . view-file-other-frame)
   ([remap find-file-read-only-other-window] . view-file-other-window)
   :map view-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("," . View-back-to-mark)
   ("g" . avy-goto-line))
  :hook prog-mode-hook)

(use-package rect
  :bind
  (
   :map rectangle-mark-mode-map
   ("i" . string-insert-rectangle)
   ("M-w" . copy-rectangle-as-kill)
   ("C-w" . kill-rectangle)
   ("C-d" . delete-rectangle)))

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
    (avy-with avy-goto-word-1
      (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))
  (with-eval-after-load 'view
    (define-keymap
      :keymap view-mode-map
      "g" #'avy-goto-line
      "f" #'avy-goto-word-crt-line)))

(use-package aggressive-indent
  :hook (after-init-hook . global-aggressive-indent-mode))

(provide 'init-edit)
