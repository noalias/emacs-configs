;;;  -*- lexical-binding: t -*-
;;;; `git'
(use-package magit
  :defer
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package git-modes
  :defer)

;;;; `rg'
(use-package rg
  :bind
  ("M-s r" . rg-menu))

(provide 'init-tools)
