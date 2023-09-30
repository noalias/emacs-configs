;;;  -*- lexical-binding: t -*-
(use-package emacs
  :init
  ;; config font
  (defvar face:font-size 23)

  (defvar face:default-fonts
    ["CaskaydiaCove NFM"
     "Fairfax HD"
     "FantasqueSansMono NFM"
     "Agave Nerd Font Mono"
     "ComicShannsMono Nerd Font Mono"
     "HarmonyOS Sans SC"
     "霞鹜文楷等宽"
     ]
    "My fonts.")

  (defvar face:emoji-font
    (cond (def:win-p "Segoe UI Emoji")
          (def:linux-p "Noto Color Emoji")))
  
  (defun face:set-font (&optional default-font-index
                                  cjk-font-index)
    (if (and default-font-index
             (length< face:default-fonts (1+ default-font-index)))
        (setq default-font-index 0))
    (if (and default-font-index
             (length< face:default-fonts (1+ cjk-font-index)))
        (setq cjk-font-index 0))
    (let ((default-font (aref face:default-fonts default-font-index))
          (cjk-font (aref face:default-fonts cjk-font-index)))
      ;; 设置 `default-font'
      (set-face-attribute 'default
		                  nil
		                  :font (font-spec :family default-font
				                           :size face:font-size))
      ;; 设置`emoji-font'
      (set-fontset-font t
		                'unicode
		                (font-spec :family face:emoji-font
			                       :size face:font-size))
      ;; 设置`cjk-font'
      (if (not (= default-font-index
                  cjk-font-index))
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font
             (frame-parameter nil 'font)
             charset
             (font-spec :family cjk-font
                        :size face:font-size))))))
  :config
  (progn ; `font'
    (setq inhibit-compacting-font-caches t)  ; Don’t compact font caches during GC.
    (face:set-font 6 6)))

(use-package doom-themes
  :config
  (load-theme 'doom-one :no-confirm)
  (setq doom-themes-enable-bold t
	    doom-themes-enable-italic t))

(use-package doom-modeline
  :hook after-init-hook
  :config
  (setq doom-modeline-major-mode-icon t
        dirvish-mode-line-height doom-modeline-height))

(provide 'init-face)
;;; init-face.el ends here
