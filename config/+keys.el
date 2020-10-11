(use-package general
  :config
  (general-evil-setup)
  (general-auto-unbind-keys))

(global-set-key (kbd "C-h C-f") #'find-function)
(global-set-key (kbd "C-g") #'evil-force-normal-state)

(general-create-definer my--leader
  :prefix "SPC")

(general-create-definer my--sub-leader
  :prefix ";")

(my--sub-leader
 :states 'normal
 :keymaps 'override
 "s" 'split-window-vertically
 "v" 'split-window-horizontally)

(my--leader
  :states 'normal
  :keymaps 'override
  ":" 'execute-extended-command
  "g g" 'magit
  "b p" 'previous-buffer
  "b n" 'next-buffer
  "C-w" 'evil-delete-backward-word
  "f s" 'save-buffer)

(provide '+keys)
