;;; -*- lexical-binding: t -*-
(use-package text-mode
  :init
  (setq-default major-mode 'text-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package latex
  :init
  (require 'tex-site)
  :mode ("\\.tex\\'" . latex-mode)
  :hook
  ((LaTeX-mode-hook . LaTeX-math-mode)
   (LaTeX-mode-hook . turn-on-reftex)
   (LaTeX-mode-hook . TeX-PDF-mode))
  :custom
  ;; use hidden dirs for auctex files
  (TeX-auto-local ".auctex-auto")
  (TeX-style-local ".auctex-style")
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
  (TeX-view-program-selection '((output-pdf "SumatraPDF")))
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

  (add-to-list 'TeX-tree-roots (no-littering-expand-var-file-name "texmf"))

  ;; Config latemk
  (add-hook 'LaTeX-mode-hook #'auctex:latexmk-setup)
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

(use-package org
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))
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
        org-reverse-note-order t))

(provide 'init-text)
;;; init-text.el ends here
