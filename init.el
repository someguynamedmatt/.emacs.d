(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))

(package-initialize)
(package-refresh-contents)

(defconst my--emacs-dir (expand-file-name user-emacs-directory)
  "The path to the .emacs.d dir")

;; hardcoded for now, replace with my--emacs-dir
(defconst my--emacs-config-dir (concat "~/.emacs.d.vanilla/" "config/")
  "The configuration files directory")

(defconst IS_MAC
  (eq system-type 'darwin)
      "Is this a Mac?")

;; If the dir(s) doesn't exist, create it
(dolist (dir (list my--emacs-config-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Push the config directory into the load path to allow for (require 'some-file)
(eval-and-compile
  (push my--emacs-config-dir load-path))

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

(use-package lsp-mode
  :config
  (add-hook 'prog-mode-hook #'lsp))

(use-package xclip
  :config
  (xclip-mode 1))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package evil
  :ensure t
  :after (general)
  :config
  (evil-mode 1)
  (use-package evil-magit
    :ensure t)
  (use-package evil-easymotion
    :ensure t
    :config
    (evilem-default-keybindings "gs"))
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
    (treemacs-follow-mode t)))

(use-package company
  :after evil
  :hook ((org-mode prog-mode) . global-company-mode)
  :init
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 3)
  :config
  (company-tng-configure-default)
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-files))

(use-package typescript-mode
  :hook (typescript-mode . rainbow-delimiters-mode))
  
(require '+core)
(require '+keys)
(require '+ivy)

(message (format "Started in %.2f seconds with %d garbage collections."
                 (float-time
                  (time-subtract after-init-time before-init-time)) gcs-done))

(defun my/reload-init ()
  "Reload the init.el file"
  (interactive)
  (load-file "~/.emacs.d.vanilla/init.el"))

(defun my/open-init ()
  "Open the init.el file"
  (interactive)
  (find-file "~/.emacs.d.vanilla/init.el"))

(defun my/treemacs-find-and-focus ()
  "Find current-buffer's file in treemacs window"
  (interactive)
  (treemacs-find-file)
  (treemacs-select-window))
