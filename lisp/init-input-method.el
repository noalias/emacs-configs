;;; -*- lexical-binding: t -*-
(use-package rime
  :bind
  (
   :map rime-active-mode-map
   ("TAB" . rime-inline-ascii)
   :map rime-mode-map
   ("M-j" . rime-force-enable)
   )
  :init
  (setq rime-user-data-dir (no-littering-expand-etc-file-name "rime/rime-ice/"))
  (let* ((name "default.custom.yaml")
         (file (expand-file-name name (no-littering-expand-etc-file-name "rime")))
         (target (expand-file-name name rime-user-data-dir)))
    (when (file-newer-than-file-p file target)
      (copy-file file target :overwrite)))
  :custom
  (default-input-method "rime")
  (rime-inline-ascii-holder ?a)
  (rime-cursor "|")
  ;; 与`rime-cursor'互斥 
  (rime-show-preedit 'inline)
  ;; (rime-title "✍️")
  (rime-inline-ascii-trigger 'shift-r)
  (rime-show-candidate 'minibuffer) ;; Options `message' `minibuffer' `posframe'
  (rime-posframe-properties (list :background-color "#333333"
                                  :foreground-color "#dcdccc"
                                  :internal-border-width 10))
  (rime-disable-predicates '(rime-predicate-prog-in-code-p))
  (rime-inline-predicates '(rime-predicate-space-after-cc-p
                            rime-predicate-after-ascii-char-p
                            rime-predicate-tex-math-or-command-p))
  :config
  (setq rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v" "," "." "S-<return>"))
  )

(provide 'init-input-method)
;;; init-input-method.el ends here
