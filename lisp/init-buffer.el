;;; -*- lexical-binding: t -*-
(use-package emacs
  :demand
  :bind
  (("C-x K" . kill-buffer-and-window)
   ("C-x x p" . switch-to-prev-buffer)
   ("C-x x n" . switch-to-next-buffer))
  :init
  (defvar buffer:skip-regexp
    (rx bos
        ?*
        (or
         "Messages"
         "Output"
         "Compile-Log"
         "Completions"
         "Flymake log"
         "Warnings"
         "Flymake diagnostics"
         "Async Shell Command"
         "Async-native-compile-log"
         "Native-compile-Log"
         "Apropos"
         "Backtrace"
         "prodigy"
         "help"
         "Calendar"
         "lsp-bridge"
         "Embark Actions"
         "Finder"
         "Kill Ring"
         "Embark Export:"
         "eshell"
         "epc con"
         "shell"
         "terminal"
         "vterm"
         "quickrun"
         "elfeed-entry"
         "macro expansion"
         "Agenda Commands"
         "Org Select"
         "Capture"
         "CAPTURE-"
         "prolog"
         "rustfmt"
         "Disabled Command"
         "straight-byte-compilation"
         "straight-process"
         )
        (* anything)
        ?*
        eos))
  :config
  (setq switch-to-prev-buffer-skip-regexp buffer:skip-regexp)
  (advice-add 'read-buffer-to-switch :around #'buffer:skip-read-buffer-to-switch)
  (defun buffer:skip-read-buffer-to-switch (fn &rest args)
    ;; 避免 `other-buffer' 选取需要忽略的buffer
    (unless (frame-parameter nil 'buffer-predicate)
      (set-frame-parameter nil 'buffer-predicate
                           (lambda (buffer)
                             (not (string-match-p buffer:skip-regexp
                                                  (buffer-name buffer))))))
    (minibuffer-with-setup-hook
        (lambda ()
          ;; 将需要忽略的buffer从 `minibuffer-completion-table' 中过滤
          (setq-local minibuffer-completion-predicate
                      (lambda (name)
                        (not (string-match-p buffer:skip-regexp
                                             (if (consp name) (car name) name))))))
      (apply fn args)))
  
  )

  (use-package ibuffer
    :bind
    ("C-x C-b" . ibuffer)
    :init
    (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
    :custom
    ;; (ibuffer-modified-char (string-to-char (nerd-icons-octicon "nf-oct-diff_modified")))
    ;; (ibuffer-read-only-char (string-to-char (nerd-icons-octicon "nf-oct-read")))
    (ibuffer-marked-char (string-to-char (nerd-icons-mdicon "nf-md-bookmark_plus")))
    (ibuffer-locked-char (string-to-char (nerd-icons-mdicon "nf-md-lock")))
    (ibuffer-deletion-char (string-to-char (nerd-icons-mdicon "nf-md-delete")))
    (ibuffer-use-other-window t)
    ;; 是否展示被过滤项目
    (ibuffer-default-display-maybe-show-predicates t))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(provide 'init-buffer)
;;; init-buffer.el ends here
