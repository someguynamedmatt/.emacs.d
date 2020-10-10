(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))
(package-initialize)
(package-refresh-contents)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-magit ivy general treemacs magit evil-leader evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(eval-when-compile
  (require 'use-package))

(use-package general
  :config
  (general-evil-setup)
  (general-auto-unbind-keys))

(use-package evil
  :ensure t
  :after (general)
  :config
  (evil-mode 1)
  (use-package evil-magit
    :ensure t)
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)))

(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))

(use-package magit
  :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (treemacs-follow-mode t))
  :bind
  (:map global-map
	("M-0" . treemacs-select-window)
	("C-x t t" . treemacs)))

(global-set-key (kbd "C-h C-f") #'find-function)
(global-set-key (kbd "C-g") #'evil-force-normal-state)
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.2)
  (setq which-key-min-display-lines 3)
  (which-key-mode))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(general-create-definer my--leader-def
  :prefix "SPC")

(my--leader-def
  :states 'normal
  :keymaps 'override
  ":" 'execute-extended-command
  "g g" 'magit
  "C-w" 'evil-delete-backward-word
  "f s" 'save-buffer)
