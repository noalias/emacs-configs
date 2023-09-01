;;;  -*- lexical-binding: t -*-
(use-package magit
  :defer
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))
  
(provide 'init-tools)
