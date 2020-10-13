(defun my/treemacs-find-and-focus ()
  "Find current-buffer's file in treemacs window"
  (interactive)
  (treemacs-find-file)
  (treemacs-select-window))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (use-package treemacs-evil
    :ensure t)
  (progn
    (treemacs-follow-mode t)))

(setq treemacs-indentation 2)
(setq treemacs-indentation-string (propertize " " 'face 'font-lock-comment-face))
(setq treemacs-follow-after-init t)
(setq treemacs-filewatch-mode t)
(setq treemacs-tag-follow-mode t)
(setq treemacs-file-event-delay 1000)

(provide '+treemacs)
