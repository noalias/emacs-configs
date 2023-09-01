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
  :config
  (setq-default TeX-master nil
                TeX-engine 'xetex)
  ;; Revert the PDF-buffer after the TeX compilation has finished
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (TeX-source-correlate-mode))

(use-package auctex-latexmk
  :hook (LaTeX-mode-hook . auctex-latexmk-setup)
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (setq TeX-command-default "LatexMk"))

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
