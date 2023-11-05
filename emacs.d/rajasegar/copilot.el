;; install dependencies
(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package editorconfig
  :ensure t)

(use-package copilot
 :load-path (lambda () (expand-file-name "elpa/copilot" user-emacs-directory)))

(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
