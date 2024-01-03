;;;  -*- lexical-binding: t -*-
;;;; `git'
(use-package magit
  :defer
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  )

(use-package vterm
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key)
              ("<C-backspace>" . (lambda () (interactive) (vterm-send-key (kbd "C-w")))))
  )

(provide 'init-tools)
