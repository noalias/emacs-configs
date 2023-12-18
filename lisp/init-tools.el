;;;  -*- lexical-binding: t -*-
;;;; `git'
(use-package magit
  :defer
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package eat
  :defer
  :custom
  (explicit-shell-file-name (executable-find "sh"))
  (eat-kill-buffer-on-exit t)
  :config
  (delete [?\C-u] eat-semi-char-non-bound-keys) ; make C-u work in Eat terminals like in normal terminals
  (delete [?\C-g] eat-semi-char-non-bound-keys) ; ditto for C-g
  (eat-update-semi-char-mode-map)
  ;; XXX: Awkward workaround for the need to call eat-reload after changing Eat's keymaps,
  ;; but reloading from :config section causes infinite recursion because :config wraps with-eval-after-load.
  (defvar eat--prevent-use-package-config-recursion nil)
  (unless eat--prevent-use-package-config-recursion
    (setq eat--prevent-use-package-config-recursion t)
    (eat-reload))
  (makunbound 'eat--prevent-use-package-config-recursion))

(provide 'init-tools)
