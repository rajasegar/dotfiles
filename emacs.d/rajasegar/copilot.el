;; install dependencies
(use-package f
  :ensure t)

(use-package editorconfig
  :ensure t)

(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))

(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(setq copilot-indent-offset-warning-disable t)

(add-to-list 'copilot-disable-display-predicates
             (lambda () (string-match-p "\\.env\\|\\.log\\|\\.gpg\\|\\.tmp$" (buffer-file-name))))   

(add-to-list 'copilot-disable-predicates
             (lambda () (memq major-mode '(shell-mode eshell-mode dired-mode))))
