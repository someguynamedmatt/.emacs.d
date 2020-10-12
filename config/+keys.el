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
(global-set-key (kbd "C-h C-f") #'describe-function)
(global-set-key (kbd "C-g") #'evil-force-normal-state)

;;
;; Leader mappings (SPC)
;;
(general-create-definer my--leader
  :prefix "SPC")

;; evil-normal mode mappings
(my--leader
  :states 'normal
  :keymaps 'override
  ":" '(execute-extended-command :which-key "M-x :")
  "g" '(:ignore t :which-key "magit")
    "g g" '(magit :which-key: "magit status")

  "b" '(:ignore t :which-key "buffers")
    "b p" '(previous-buffer :which-key "previous buffer")
    "b n" '(next-buffer :which-key "next buffer")
    "b s" '(save-buffer :which-key "save buffer")

  "C-w" '(evil-delete-backward-word :which-key "delete word backwards")
  "i" '(:ignore t :which-key "init file")
    "i r" '(my/reload-init :which-key "reload init.el")
    "i o" '(my/open-init :which-key "open init.el"))

;; evil-motion mode mappings (visual, normal, etc.)
(my--leader
  :states 'motion
  :keymaps 'override
  "t" '(:ignore t :which-key "treemacs")
    "t t" '(treemacs :which-key "toggle treemacs")
    "t f" '(treemacs :which-key "target file"))


;;
;; Sub-leader mappings (;)
;;
(general-create-definer my--sub-leader
  :prefix ";")

;; evil-normal mode mappings
(my--sub-leader
 :states 'normal
 :keymaps 'override
 "s" '(split-window-vertically :which-key "split vertically")
 "v" '(split-window-horizontally :which-key "split horizontally"))

;; evil-motion mode mappings (visual, normal, etc.)
(my--sub-leader
 :states 'motion
 :keymaps 'override
 "n" '(treemacs :which-key "toggle treemacs")
 "f" '(treemacs-select-window :which-key "focus treemacs window"))


(provide '+keys)
