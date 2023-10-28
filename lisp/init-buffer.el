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
        (? ?*)
        (or
         "Messages"
         (seq (or ?O ?o) "utput")
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
         "Calendar"
         "Finder"
         "Kill Ring"
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
         "magit-process: "
         "magit-diff: "
         )
        (* anything)
        (? ?*)
        eos))
  (defvar buffer:skip-mode '(Tex-Output-mode))
  :config
  (setq switch-to-prev-buffer-skip-regexp buffer:skip-regexp)
  (advice-add 'read-buffer-to-switch :around #'buffer:skip-read-buffer-to-switch)
  (defun buffer:skip-read-buffer-to-switch (fn &rest args)
    ;; 避免 `other-buffer' 选取需要忽略的buffer
    (set-frame-parameter nil 'buffer-list
                         (seq-filter (lambda (buffer)
                                       (buffer-match-p
                                        (or (list buffer:skip-regexp
                                                  (and (mapcar
                                                        (lambda (item) (cons 'major-mode item))
                                                        buffer:skip-mode))))
                                        buffer))
                                     (frame-parameter nil 'buffer-list)))

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
