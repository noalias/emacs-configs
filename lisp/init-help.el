;;; -*- lexical-binding: t -*-
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
