(use-package flycheck-pos-tip)

(use-package flycheck
  :config
  (setq flycheck-standard-error-navigation t)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (when (display-graphic-p)
    (flycheck-pos-tip-mode)))

(provide '+flycheck)
    
    
