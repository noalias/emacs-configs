;;; -*- lexical-binding: t -*-
(use-package emacs
  :demand
  :bind
  (("C-x K" . kill-buffer-and-window)
   ("C-x x p" . switch-to-prev-buffer)
   ("C-x x n" . switch-to-next-buffer)))

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer-jump)
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :custom
  (ibuffer-use-other-window t)
  ;; 是否展示被过滤项目
  (ibuffer-default-display-maybe-show-predicates t))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(use-package tabspaces
  :hook after-init-hook
  :custom
  (tabspaces-keymap-prefix "C-c t"))

(provide 'init-buffer)
;;; init-buffer.el ends here
