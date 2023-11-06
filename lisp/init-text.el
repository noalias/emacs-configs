;;; -*- lexical-binding: t -*-
(use-package text-mode
  :init
  (setq-default major-mode 'text-mode))

(use-package csv-mode
  :if def:win-p
  :mode "\\.ptd\\'"
  :config
  (add-hook 'csv-mode-hook
            (lambda ()
              (when (string-match-p "\\.ptd\\'" (buffer-file-name))
                (csv-set-comment-start "!")
                (csv-set-separator (string-to-char " "))))))

(use-package conf-mode
  :if def:win-p
  :mode ("\\.pro\\'" . conf-space-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package latex
  :mode ("\\.tex\\'" . latex-mode)
  :hook
  ((LaTeX-mode-hook . LaTeX-math-mode)
   (LaTeX-mode-hook . turn-on-reftex)
   (LaTeX-mode-hook . TeX-PDF-mode))
  :custom
  ;; use hidden dirs for auctex files
  (TeX-check-TeX nil)
  ;; Show output of Tex compilation in other window.
  ;; (TeX-show-compilation t)
  ;; Automatically save style information when saving the buffer.
  (TeX-auto-save t)
  ;; Parse file after loading it if no style hook is found for it.
  (TeX-parse-self t)
  ;; Automatically untabify when saving the buffer.
  (TeX-auto-untabify t)
  ;; If non-nil, ask user for permission to save files before starting TeX.
  (TeX-save-query nil)
  (TeX-electric-math '("$" . "$"))
  ;; view by SumatraPDF
  (TeX-view-program-selection `((output-pdf ,def:pdf-program)))
  ;; Control if server should be started for inverse search.
  (TeX-source-correlate-start-server t)
  ;; Style
  (LaTeX-default-style "standalone")
  (reftex-plug-into-AUCTeX t)
  (TeX-clean-confirm nil)
  :config
  (setq-default TeX-master nil
                TeX-engine 'xetex
                TeX-output-dir "build")
  ;; Revert the PDF-buffer after the TeX compilation has finished
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (TeX-source-correlate-mode)

  ;; 配置本地包路径
  ;; (add-to-list 'TeX-tree-roots (base:expand-req-file-name "texmf"))
  ;; 无法通过该设置使编译器查找到`texmf'.
  ;; 只能通过`tlmgr conf texmf TEXMFHOME /path/to/package'进行设置。

  ;; Config latemk
  (defun TeX-command-master-run-latexmk (&optional override-confirm)
    "Run command on the current document.

If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
depend on it being positive instead of the entry in `TeX-command-list'."
    (interactive "P")
    (TeX-master-file nil nil t)  ;; call to ask if necessary
    (TeX-command "LaTexmk" #'TeX-master-file override-confirm))

  (when (executable-find "latexmk")
    (add-hook 'LaTeX-mode-hook #'auctex:latexmk-setup)
    (define-keymap
      :keymap LaTeX-mode-map
      "C-c C-c" #'TeX-command-master-run-latexmk
      "C-c C-x" #'TeX-command-master))

  (defun auctex:latexmk-setup ()
    (setq TeX-command-default "LaTexmk"
          TeX-command-extra-options (cond
                                     ((eq TeX-engine 'xetex)"-shell-escape")
                                     (t ""))
          LaTeX-clean-intermediate-suffixes
          (append LaTeX-clean-intermediate-suffixes '("\\.fdb_latexmk" "\\.aux.bak" "\\.fls")))
    (add-to-list 'TeX-expand-list
                 '("%(-PDF)"
                   (lambda ()
                     (cond
                      ((and (eq TeX-engine 'default)
                            TeX-PDF-mode)
                       "-pdf")
                      ((eq TeX-engine 'xetex) "-xelatex ")
                      (t "")))))
    (add-to-list 'TeX-command-list
                 '("LaTexmk"
                   "latexmk %(-PDF) %(mode) %(file-line-error) %(extraopts) %(output-dir) %S%(PDFout) %t"
                   TeX-run-format
                   nil
                   (latex-mode doctex-mode)
                   :help "Run Latexmk")))

  ;; See: https://github.com/tom-tan/auctex-latexmk/issues/28
  ;;   &  https://stackoverflow.com/questions/3124273/compile-xelatex-tex-file-with-latexmk
  (advice-add #'TeX-output-extension :before #'auctex:latexmk--TeX-output-extension)
  (defun auctex:latexmk--TeX-output-extension ()
    (when (and TeX-PDF-mode
               (eq TeX-engine 'xetex)
               (string-match-p "latexmk" TeX-command-default))
      (unless (listp TeX-output-extension)
        (setq TeX-output-extension (list TeX-output-extension)))))
  )

(use-package cdlatex
  :hook ((LaTeX-mode-hook . turn-on-cdlatex)
         (latex-mode-hook . turn-on-cdlatex)))

(use-package org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-startup-truncated t)
  :config
  (require 'org-tempo)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-targets `((nil :level . 1)
                             ("emacs.org" :level . 1)
                             ("work.org" :level . 1)
                             ("toys.org" :level . 1)
                             ("projects.org" :level . 1))
        ;; `entry' 放置在子节点首位
        org-reverse-note-order t)
  (when (fboundp 'turn-on-cdlatex)
    (add-hook 'org-mode-hook #'org-cdlatex-mode)))

(use-package org-capture
  :bind
  ("C-c c" . org-capture))

(use-package denote
  :init
  (defvar denote:collection-directory (expand-file-name "~/Reference/"))
  (defvar-keymap denote:command-map
    "d" #'denote-open-or-create
    "r" #'denote-rename-file)
  (fset 'denote:command-map denote:command-map)
  :hook ((dired-mode-hook . denote-dired-mode)
         (after-init-hook . denote-modules-global-mode))
  :bind
  ("C-c d" . denote:command-map)
  :custom
  (denote-directory "~/notes/"))

(use-package bibtex
  :defer
  :config
  (add-to-list 'bibtex-biblatex-entry-alist
               '("Standard" "Standards Information of Chinese"
                 (("author")
                  ("title")
                  ("date" nil nil 1)
                  ("year" nil nil -1))
                 nil
                 (("number")
                  ("organization")
                  ("publisher")
                  ("status")
                  ("url")
                  ("language")))))

(use-package ebib
  :bind
  ("C-c e" . ebib)
  :custom
  (ebib-preload-bib-files '("~/Reference/index.bib"))
  (ebib-bibtex-dialect 'biblatex)
  (ebib-default-directory "~/Reference/")
  :config
  (setf (alist-get "pdf" ebib-file-associations
                   nil
                   :remove
                   'string=)
        def:pdf-program))

(use-package ebib-notes
  :autoload ebib-notes-create-org-template
  :custom
  (ebib-notes-use-org-capture "e"))

(provide 'init-text)
;;; init-text.el ends here
