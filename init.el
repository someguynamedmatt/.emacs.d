(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))
(package-initialize)
;; NOTE: this isn't explicitly necessary, it can be done manually.
;; Having it enabled slows down startup while the packages refresh.
;;(package-refresh-contents)


(setq user-full-name "Matt Young"
      user-mail-address "dev@mttyng.com")


(defconst my--emacs-dir (expand-file-name "~/.emacs.d.vanilla/")
  "The path to the .emacs.d dir")


;; hardcoded for now, replace with my--emacs-dir
(defconst my--emacs-config-dir (concat my--emacs-dir "config/")
  "The configuration files directory")

;; This puts the custom-set-variables in the custom.el
;; rather than polluting this file.
(setq custom-file (expand-file-name "+custom.el" my--emacs-config-dir))
(when (file-exists-p custom-file) (load custom-file))


(defconst IS_MAC
  (eq system-type 'darwin)
      "Is this a Mac?")


(if (eq IS_MAC t)
    (exec-path-from-shell-initialize))


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

(use-package helpful)

(use-package rg)

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq backward-delete-char-untabify-method 'hungry)

(use-package xclip
  :config
  (xclip-mode 1))

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-method 'bitmap))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))

(use-package magit
  :ensure t)

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

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/code"))
  (projectile-mode +1))

(defmacro after (feature &rest body)
  "Executes BODY after FEATURE has been loaded.
    FEATURE may be any one of:
    'evil            => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]     => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))"
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (cl-loop for f across feature
               do
               (progn
                 (setq prog (append `(',f) `(,prog)))
                 (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))
  
(use-package doom-themes
  :config
  (use-package doom-modeline
    :config
    (doom-modeline-mode 1)
    (setq doom-modeline-buffer-modification-icon nil)
    (setq doom-modeline-percent-position nil)
    (setq line-number-mode nil)
    (setq doom-modeline-buffer-encoding nil))
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-Iosvkem t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(require '+core)
(require '+evil)
(require '+treemacs)
(require '+lsp)
(require '+keys)
(require '+ivy)
(require '+flycheck)
(require '+typescript)
(require '+crosshairs)
 
(add-hook 'prog-mode-hook 'linum-mode)

(message (format "Started in %.2f seconds with %d garbage collections."
                 (float-time
                  (time-subtract after-init-time before-init-time))
                 gcs-done))

(defun my/reload-init ()
  "Reload the init.el file"
  (interactive)
  (load-file "~/.emacs.d.vanilla/init.el"))

(defun my/open-init ()
  "Open the init.el file"
  (interactive)
  (find-file "~/.emacs.d.vanilla/init.el"))

(defun my/open-keys ()
  "Open the +keys.el file"
  (interactive)
  (find-file "~/.emacs.d.vanilla/config/+keys.el"))

(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun my/backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (my/delete-word (- arg)))

;; Display the time in the modeline
(display-time-mode 1)
