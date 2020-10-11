(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))
(package-initialize)
(package-refresh-contents)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(treemacs-evil evil-magit ivy general treemacs magit evil-leader evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(eval-when-compile
  (require 'use-package))

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
  (use-package treemacs-evil
    :ensure t)
  (progn
    (treemacs-follow-mode t))
  :bind
  (:map global-map
	("M-0" . treemacs-select-window)
	("C-x t t" . treemacs)))

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

(load "~/.emacs.d/config/+keys")
