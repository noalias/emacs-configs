;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq inhibit-splash-screen 1)
  )

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

(eval-and-compile ;; `use-package'
  (require 'use-package)
  ;; `use-package' disable omit `-hook'
  (setq use-package-hook-name-suffix nil)
  (setq use-package-verbose t))

(use-package borg
  :load-path "lib/borg"
  :config
  (add-to-list 'load-path (expand-file-name "lib/auctex" user-emacs-directory))
  (borg-initialize))

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

;;; Long tail
(use-package emacs
  :load-path ("lisp" "site-lisp")
  :init
  (use-package init-def)
  (use-package init-base)
  :config
  (use-package init-tools)
  (use-package init-completion)
  (use-package init-buffer)
  (use-package init-dired)
  (use-package init-image)
  (use-package init-face)
  (use-package init-window)
  (use-package init-edit)
  (use-package init-input-method)
  (use-package init-text)
  (use-package init-prog)
  (use-package init-help)
  (use-package init-file)
  )

;;; Tequila worms

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

;;; init.el ends here
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
