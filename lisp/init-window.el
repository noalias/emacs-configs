;;;  -*- lexical-binding: t -*-
;;; `display-buffer-alist' config
(use-package emacs
  :demand t
  ;; :init
  ;;   (defvar window:help-modes-list '(help-mode
  ;;                                    helpful-mode
  ;;                                    eldoc-mode
  ;;                                    Info-mode)
  ;;     "List of major-modes used in documentation buffers")
  ;;   (defun window:buffer-mode (&optional buffer-or-name)
  ;;     "Returns the major mode associated with a buffer.
  ;; If buffer-or-name is nil return current buffer's mode."
  ;;     (buffer-local-value 'major-mode
  ;;                         (if buffer-or-name
  ;;                             (get-buffer buffer-or-name)
  ;;                           (current-buffer))))
  :custom
  ;;   ;; display-buffer-action-functions are:
  ;;   ;;  `display-buffer-same-window' -- Use the selected window.
  ;;   ;;  `display-buffer-reuse-window' -- Use a window already showing the buffer.
  ;;   ;;  `display-buffer-in-previous-window' -- Use a window that did show the buffer before.
  ;;   ;;  `display-buffer-use-some-window' -- Use some existing window.
  ;;   ;;  `display-buffer-pop-up-window' -- Pop up a new window.
  ;;   ;;  `display-buffer-below-selected' -- Use or pop up a window below the selected one.
  ;;   ;;  `display-buffer-at-bottom' -- Use or pop up a window at the bottom of the selected frame.
  ;;   ;;  `display-buffer-pop-up-frame' -- Show the buffer on a new frame.
  ;;   ;;  `display-buffer-in-child-frame' -- Show the buffer in a child frame.
  ;;   ;;  `display-buffer-no-window' -- Do not display the buffer and have `display-buffer' return nil immediately.

  ;;   ;; Action alist entries are:
  ;;   ;;  `inhibit-same-window' -- A non-nil value prevents the same
  ;;   ;;     window from being used for display.
  ;;   ;;  `inhibit-switch-frame' -- A non-nil value prevents any frame
  ;;   ;;     used for showing the buffer from being raised or selected.
  ;;   ;;  `reusable-frames' -- The value specifies the set of frames to
  ;;   ;;     search for a window that already displays the buffer.
  ;;   ;;     Possible values are nil (the selected frame), t (any live
  ;;   ;;     frame), visible (any visible frame), 0 (any visible or
  ;;   ;;     iconified frame) or an existing live frame.
  ;;   ;;  `pop-up-frame-parameters' -- The value specifies an alist of
  ;;   ;;     frame parameters to give a new frame, if one is created.
  ;;   ;;  `window-height' -- The value specifies the desired height of the
  ;;   ;;     window chosen and is either an integer (the total height of
  ;;   ;;     the window), a floating point number (the fraction of its
  ;;   ;;     total height with respect to the total height of the frame's
  ;;   ;;     root window) or a function to be called with one argument -
  ;;   ;;     the chosen window.  The function is supposed to adjust the
  ;;   ;;     height of the window; its return value is ignored.  Suitable
  ;;   ;;     functions are `shrink-window-if-larger-than-buffer' and
  ;;   ;;     `fit-window-to-buffer'.
  ;;   ;;  `window-width' -- The value specifies the desired width of the
  ;;   ;;     window chosen and is either an integer (the total width of
  ;;   ;;     the window), a floating point number (the fraction of its
  ;;   ;;     total width with respect to the width of the frame's root
  ;;   ;;     window) or a function to be called with one argument - the
  ;;   ;;     chosen window.  The function is supposed to adjust the width
  ;;   ;;     of the window; its return value is ignored.
  ;;   ;;  `preserve-size' -- The value should be either (t . nil) to
  ;;   ;;     preserve the width of the chosen window, (nil . t) to
  ;;   ;;     preserve its height or (t . t) to preserve its height and
  ;;   ;;     width in future changes of the window configuration.
  ;;   ;;  `window-parameters' -- The value specifies an alist of window
  ;;   ;;     parameters to give the chosen window.
  ;;   ;;  `allow-no-window' -- A non-nil value means that `display-buffer'
  ;;   ;;     may not display the buffer and return nil immediately.
  (display-buffer-alist
   '(
     ;; Hide the mode line of the Completions buffers
     ("\\`\\*Completions\\*"
      nil
      (window-parameters (mode-line-format . none))))

   ((lambda (buffer-or-name _)
      (let ((buffer (get-buffer buffer-or-name)))
        (with-current-buffer buffer
          (or (equal major-mode 'vterm-mode)
              (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
    (display-buffer-reuse-window display-buffer-at-bottom)
    ;;(display-buffer-reuse-window display-buffer-in-direction)
    ;;display-buffer-in-direction/direction/dedicated is added in emacs27
    ;;(direction . bottom)
    ;;(dedicated . t) ;dedicated is supported in emacs27
    (reusable-frames . visible)
    (window-height . 0.3)
    (window-parameters (mode-line-format . none)))
   )
  ;;      ;; Help modes config
  ;;      ((lambda (buf) (member (window:buffer-mode buf) window:help-modes-list))
  ;;       (display-buffer-reuse-window
  ;;        display-buffer-in-direction
  ;;        display-buffer-in-side-window)
  ;;       (body-function . select-window)
  ;;       ;; (direction . bottom)
  ;;       ;; (window-height . (lambda (win) (fit-window-to-buffer win 25 14)))
  ;;       (window-width . 77 ;; (lambda (win) (fit-window-to-buffer win nil nil 75 65))
  ;;                     )
  ;;       (direction . right)
  ;;       (side . right)
  ;;       (window-parameters . ((split-window . #'ignore))))
  ;;      )
  ;;    )
  
  )

;;; Tab bar
(use-package tab-bar
  :hook (after-init-hook . tab-bar-history-mode)
  :bind
  (("M-<right>" . tab-bar-switch-to-next-tab)
   ("M-<left>" . tab-bar-switch-to-prev-tab))
  :custom
  (tab-bar-switch-to 'left)
  :config
  (setq tab-bar-new-button nil
        tab-bar-border nil
        tab-bar-close-button nil
        tab-bar-back-button nil
        tab-bar-tab-name-truncated-max 10)
  :custom-face
  (tab-bar ((t (:inherit hl-line))))
  (tab-bar-tab ((t (:inverse-video t :bold t))))
  (tab-bar-tab-inactive ((t (:inherit shadow)))))

(provide 'init-window)
;;; init-window.el ends here
