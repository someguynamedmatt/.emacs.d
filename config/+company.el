(use-package company
  :after evil
  :hook ((org-mode prog-mode) . global-company-mode)
  :init
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 3)
  :config
  (company-tng-configure-default)
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-lsp)
  (add-to-list 'company-backends 'company-files))

(provide '+company)
