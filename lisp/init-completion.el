;;;  -*- lexical-binding: t -*-
(use-package minibuffer
  :bind
  (:map minibuffer-local-completion-map
        ("C-n" . minibuffer-next-completion)
        ("C-p" . minibuffer-previous-completion)
        ("C-RET" . completion:force-exit)
        ("SPC")
        :map completion-list-mode-map
	    ("z" . switch-to-minibuffer))
  :custom
  (minibuffer-electric-default-mode t)
  ;; Don't insert completion at point into minibuffer
  ;; `M-<RET>' complete `minibuffer'
  (minibuffer-completion-auto-choose nil)
  ;; One frame one minibuffer.
  (minibuffer-follows-selected-frame nil)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (minibuffer-default-prompt-format " [%s]")
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (resize-mini-windows t)
  (completion-auto-help 'always)
  (completion-show-help nil)
  (completion-show-inline-help nil)
  (completion-cycle-threshold nil)
  ;; `t' `second-tab' `nil'
  (completion-auto-select 'second-tab)
  (completions-detailed t)
  ;; Ignore cases when complete
  (completion-ignore-case t)
  ;; vertical display
  (completions-format 'one-column)
  (completions-max-height 20)
  (completions-sort #'completion:list-sort)
  :config
  (defun completion:list-sort (all)
    "对 `Completions-buffer' 中的补全项进行排序"
    (let ((hist (minibuffer-history-value)))
      (thread-first all
                    (sort (lambda (c1 c2) (< (length c1) (length c2))))
                    (sort (lambda (c1 c2) (> (length (member c1 hist))
                                         (length (member c2 hist))))))))
  ;; Copy from `icomplete'
  (defun completion:force-exit (force)
    "Attempt to exit minibuffer immediately with current input.
Unless FORCE is non-nil (interactively with a prefix argument),
honor a non-nil REQUIRE-MATCH argument to `completing-read' by
trying to complete as much as possible and disallowing the exit
if that doesn't produce a completion match."
    (interactive "P")
    (if (and (not force) minibuffer--require-match)
        (minibuffer-complete-and-exit)
      (exit-minibuffer)))
  )

(use-package aggressive-completion
  :hook after-init-hook
  :custom
  (aggressive-completion-delay 0.4))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :demand t
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap isearch-forward] . consult-line)
   ([remap Info-search] . consult-info)
   ([remap repeat-complex-command] . consult-complex-command)
   ("M-g o" . consult-outline)
   ("M-g I" . consult-imenu-multi)
   ("M-g m" . consult-mark)
   ("M-s f" . consult-fd)
   ("M-s g" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-debounce 0.1
        consult-async-input-throttle 0.2
        consult-narrow-key "<"
        consult-line-number-widwn t)
  )

(use-package consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file))
  :config
  (setq consult-dir-default-command 'consult-dir-dired))

(use-package company
  :hook
  (after-init-hook . global-company-mode)
  :custom
  (company-backends '((company-capf :with company-yasnippet)
                      (company-keywords company-files)))
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 12)
  (company-tooltip-offset-display 'line)
  :config
  (setq company-global-modes '(not message-mode
                                   help-mode
                                   eshell-mode
                                   shell-mode))
  (setq company-idle-delay 0
        company-minimum-prefix-length 3
        company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

(use-package consult-company
  :after (company consult)
  :bind
  (
   :map company-mode-map
   ([remap completino-at-point] . consult-company)
   :map company-active-map
   ([remap company-search-candidates] . consult-company))
  :config
  (consult-customize consult-company
                     :initial company-prefix))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package rg
  :bind
  ("M-s s" . rg-autoload-keymap)
  :config
  (defun rg-autoload-keymap ()
    (interactive)
    (if (not (require 'rg nil t))
        (user-error (format "Cannot load rg"))
      (let ((key-vec (this-command-keys-vector)))
        (global-set-key key-vec rg-global-map)
        (setq unread-command-events
              (mapcar (lambda (ev) (cons t ev))
                      (listify-key-sequence key-vec)))))))

(provide 'init-completion)
;;; init-completion.el ends here
