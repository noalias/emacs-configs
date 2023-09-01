;;; -*- lexical-binding: t -*-
;; (use-package helpful
;;   :vc
;;   :bind
;;   (([remap describe-variable] . helpful-variable)
;;    ([remap describe-symbol] . helpful-symbol)
;;    ([remap describe-function] . helpful-callable)
;;    ([remap describe-command] . helpful-command)
;;    ([remap Info-goto-emacs-command-node] . helpful-function)
;;    ([remap describe-key] . helpful-key)
;;    ("C-h SPC" . helpful-at-point)))

(use-package apropos
  :defer t
  :config
  (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
    (button-type-put
     fun-bt 'action
     (lambda (button)
       (helpful-callable (button-get button 'apropos-symbol)))))
  (dolist (var-bt '(apropos-variable apropos-user-option))
    (button-type-put
     var-bt 'action
     (lambda (button)
       (describe-variable (button-get button 'apropos-symbol))))))

(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

(provide 'init-help)
;;; init-help.el ends here
