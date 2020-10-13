(use-package flycheck-pos-tip)

(use-package flycheck
  :config
  (setq flycheck-standard-error-navigation t)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (when (display-graphic-p)
    (setq flycheck-pos-tip-timeout -1)
    (flycheck-pos-tip-mode)))

(provide '+flycheck)
    
    
