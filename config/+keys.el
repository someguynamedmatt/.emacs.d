(use-package general
  :config
  (general-evil-setup)
  (general-auto-unbind-keys))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.2)
  (setq which-key-min-display-lines 4)
  (which-key-mode))

;;
;; Custom mappings
;;

;; Global mappings
(global-set-key (kbd "C-h C-f") #'helpful-function)
(global-set-key (kbd "C-h C-d") #'helpful-at-point)
(global-set-key (kbd "C-g") #'evil-force-normal-state)
(global-set-key (kbd "C-w") #'my/backward-delete-word)

(general-define-key
 :states 'normal
 "*" '(swiper-thing-at-point :which-key "swiper at point")
 "/" '(swiper :which-key "swiper"))


(general-create-definer my--file-map
  :prefix "C-f")

(my--file-map
 :states 'normal
 :keymaps 'override
 "C-f" '(+lookup/file :which-key "goto file"))


;;
;; Leader mappings (SPC)
;;
(general-create-definer my--leader
  :prefix "SPC")

;; evil-normal mode mappings
(my--leader
  :states 'normal
  :keymaps 'override
  "/" '(evilnc-comment-or-uncomment-lines :which-key "comment toggle")
  "," '(ibuffer :which-key "ibuffer")
  "p" '(projectile-command-map :which-key "projectile")
  "SPC" '(projectile-find-file :which-key "projectile find file")
  "g" '(:ignore t :which-key "magit")
    "g g" '(magit :which-key "magit status")

  "f" '(:ignore t :which-key "file")
    "f s" '(save-buffer :which-key "save file")
  "b" '(:ignore t :which-key "buffers")
    "b s" '(save-buffer :which-key "save buffer")

  "i" '(:ignore t :which-key "init file")
    "i r" '(my/reload-init :which-key "reload init.el")
    "i k" '(my/open-keys :which-key "open config/+keys.el")
    "i o" '(my/open-init :which-key "open init.el"))

;; evil-motion mode mappings (visual, normal, etc.)
(my--leader
  :states 'motion
  :keymaps 'override
  ":" '(counsel-M-x :which-key "M-x :")
  "b" '(:ignore t :which-key "buffers")
    "b p" '(previous-buffer :which-key "previous buffer")
    "b n" '(next-buffer :which-key "next buffer")

  "t" '(:ignore t :which-key "treemacs")
    "t t" '(treemacs :which-key "toggle treemacs")
    "t f" '(treemacs :which-key "target file")

  "1" '(winum-select-window-1 :which-key "select window 1")
  "2" '(winum-select-window-2 :which-key "select window 2")
  "3" '(winum-select-window-3 :which-key "select window 3")
  "4" '(winum-select-window-4 :which-key "select window 4")
  "5" '(winum-select-window-5 :which-key "select window 5")
  "6" '(winum-select-window-6 :which-key "select window 6")
  "7" '(winum-select-window-7 :which-key "select window 7"))


(my--leader
  :states 'visual
  :keymaps 'override
  "/" '(comment-or-uncomment-region :which-key "comment toggle"))


;;
;; Sub-leader mappings (;)
;;
(general-create-definer my--sub-leader
  :prefix ";")

;; evil-normal mode mappings
(my--sub-leader
 :states 'normal
 :keymaps 'override
 "SPC" '(evil-ex-nohighlight :which "no highlight")
 "s" '(split-window-vertically :which-key "split vertically")
 "v" '(split-window-horizontally :which-key "split horizontally"))

;; evil-motion mode mappings (visual, normal, etc.)
(my--sub-leader
 :states 'motion
 :keymaps 'override
 "a" '(+ivy/project-search :which-key "search project")
 "n" '(treemacs :which-key "toggle treemacs")
 "f" '(treemacs-select-window :which-key "focus treemacs window"))


(provide '+keys)
