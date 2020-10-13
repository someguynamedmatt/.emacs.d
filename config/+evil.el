(use-package evil
  :defer .1
  :after (general)
  :config
  (evil-mode 1)
  (use-package evil-magit)
  (use-package evil-nerd-commenter)
  (use-package evil-easymotion
    :ensure t
    :config
    (evilem-default-keybindings "gs"))
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)))

(provide '+evil)
