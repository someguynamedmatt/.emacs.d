;;; +typescript.el -*- lexical-binding: t; -*-

(use-package typescript-mode
  :hook (typescript-mode . rainbow-delimiters-mode)
  :config
  (flycheck-mode +1))

(use-package web-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))))

(add-hook 'web-mode-hook #'my/lsp-activate)

(defun my/enable-prettier-js-mode ()
  "Maybe enable `prettier-js-mode'.  Used with `web-mode-hook'."
  (unless (and buffer-file-name
               (string-match-p "html?" (file-name-extension buffer-file-name)))
    (prettier-js-mode 1)))

(after 'web-mode (define-derived-mode typescript-tsx-mode web-mode "typescript-tsx")
       (add-hook 'typescript-mode-hook #'my/lsp-activate)
       (add-hook 'typescript-mode-hook #'(message "LSP ACTIVE")))

(eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'my/enable-prettier-js-mode)))


(eval-after-load 'typescript-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'my/enable-prettier-js-mode)))


(eval-after-load 'typescript-tsx-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'my/enable-prettier-js-mode)))


(setq prettier-js-show-errors "buffer")


(provide '+typescript)
