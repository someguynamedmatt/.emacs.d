(use-package lsp-mode
  :ensure t
  :commands lsp)

(defun my/lsp-activate ()
  (interactive)
  (setq lsp-prefer-flymake nil)
  (setq lsp-log-io nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-ui-doc-enable t)
  (lsp))

(provide '+lsp)
  
