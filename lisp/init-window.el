;;;  -*- lexical-binding: t -*-
;;; `display-buffer-alist' config
(use-package emacs
  :custom
  (display-buffer-alist
   `(
     ;; Hide the mode line of the Completions buffers
     ("\\`\\*Completions\\*"
      nil
      (window-parameters (mode-line-format . none)))
     ;; Message buffer
     (,(rx bos ?*
           (or "Message" "Warnings" "Shell Command Output")
           (* anything)
           ?*
           eos)
      (display-buffer-reuse-window display-buffer-in-side-window)
      (reusable-frames . visible)
      (side . bottom)
      (window-height . 0.33)
      (body-function . select-window))
     ;; Help display
     ((or (major-mode . help-mode)
          (major-mode . Info-mode))
      (display-buffer-reuse-mode-window display-buffer-in-side-window)
      (reusable-frames . visible)
      (mode (help-mode Info-mode))
      (side . right)
      (window-width . 0.43)
      (body-function . select-window))
     ;; rg-mode display
     ((or (major-mode . rg-mode)
          (major-mode . occur-mode))
      (display-buffer-reuse-window display-buffer-in-side-window)
      (reusable-frames . visible)
      (side . right)
      (window-width . 0.5)
      (body-function . select-window))
     )
   )
  )

;;; Tab bar
(use-package tab-bar
  :hook (after-init-hook . tab-bar-history-mode)
  :bind
  (("M-<right>" . tab-bar-switch-to-next-tab)
   ("M-<left>" . tab-bar-switch-to-prev-tab))
  :custom
  (tab-bar-switch-to 'left)
  :config
  (setq tab-bar-new-button nil
        tab-bar-border nil
        tab-bar-close-button nil
        tab-bar-back-button nil
        tab-bar-tab-name-truncated-max 10)
  :custom-face
  (tab-bar ((t (:inherit hl-line))))
  (tab-bar-tab ((t (:inverse-video t :bold t))))
  (tab-bar-tab-inactive ((t (:inherit shadow)))))

(provide 'init-window)
;;; init-window.el ends here
