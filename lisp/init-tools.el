;;;  -*- lexical-binding: t -*-

;;;; `git'
(use-package magit
  :defer
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package git-modes :defer)

(provide 'init-tools)
