;;;  -*- lexical-binding: t -*-
;;; `treesit'
(use-package treesit-auto
  :if (treesit-available-p)
  :init
  (defvar treesit:load-path (no-littering-expand-var-file-name "tree-sitter")
    "The directory where install compile and load a tree-sitter language grammar library.")
  :hook
  (after-init-hook . global-treesit-auto-mode)
  :custom
  (treesit-font-lock-level 4)
  (treesit-auto-install t)
  :config
  (add-to-list 'treesit-extra-load-path treesit:load-path)
  (defun treesit:patch--path (fn &rest arg)
    (unless (nth 0 arg)
      (setf (car arg) treesit:load-path))
    (apply fn arg))
  (advice-add #'treesit--install-language-grammar-1 :around #'treesit:patch--path)
  )

;;; `prog-mode'
(use-package prog-mode
  :defer
  :config
  (add-hook 'prog-mode-hook #'global-prettify-symbols-mode)
  (add-hook 'prog-mode-hook #'flymake-mode)
  (add-hook 'prog-mode-hook #'eldoc-mode)
  
  (defun prog:indent-spaces-mode ()
    (setq indent-tabs-mode nil)))

;;;; `elisp-mode'
(use-package elisp-mode
  :defer
  :config
  (add-hook 'emacs-lisp-mode-hook #'reveal-mode)
  (add-hook 'lisp-interaction-mode-hook #'prog:indent-spaces-mode))

;;;; `elvish'
(use-package elvish-mode :defer)

;;;; `rust-mode'
(use-package rust-ts-mode
  :mode "\\.rs\\'")

(use-package cargo
  :hook (rust-ts-mode-hook . cargo-minor-mode)
  :custom
  (compilation-ask-about-save nil)
  :config
  (define-key cargo-mode-map (kbd "C-c") 'cargo-minor-mode-command-map))

;;;; `typescript-ts-mode'
(use-package typescript-ts-mode
  :mode "\\.ts\\'")

;;;; `javascript-ts-mode'
(use-package js-ts-mode
  :mode "\\.js\\'")

;;;; `lua-mode'
(use-package lua-mode :defer)

;;;; `scad-mode' to edit OpenSCAD files
(use-package scad-mode :defer)

;;; `yaml-ts-mode'
(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

;;; `yasnippet'
(use-package yasnippet
  :hook (after-init-hook . yas-global-mode))

;;; `eglot'
(use-package eglot
  :hook
  ((rust-ts-mode-hook . eglot-ensure)
   (typescript-ts-mode-hook . eglot-ensure)
   (js-ts-mode-hook . eglot-ensure)
   (scad-mode-hook . eglot-ensure))
  :config
  (progn ;; `deno'
    (add-to-list 'eglot-server-programs
                 '((js-ts-mode typescript-ts-mode) . (eglot-deno "deno" "lsp")))

    (defclass eglot-deno (eglot-lsp-server) ()
      :documentation "A custom class for deno lsp.")

    (cl-defmethod eglot-initialization-options ((server eglot-deno))
      "Passes through required deno initialization options."
      (list :enable t
            :lint t)))
  
  (progn ;; `openscad-lsp'
    (add-to-list 'eglot-server-programs
                 '((scad-mode) . (eglot-openscad "openscad-lsp")))
    
    (defclass eglot-openscad (eglot-lsp-server) ()
      :documentation "A custom class for openscad lsp.")
    
    (cl-defmethod eglot-initialization-options ((server eglot-openscad))
      "Passes through required openscad-lsp initialization options."
      (list :search_paths ""
            :fmt_style "file"
            :default_param t))
    )
  )

;;;; `other'
(provide 'init-prog)
