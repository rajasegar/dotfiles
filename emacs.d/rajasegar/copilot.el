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

;; (use-package copilot
 ;; :load-path (lambda () (expand-file-name "elpa/copilot" user-emacs-directory)))

;; (add-hook 'prog-mode-hook 'copilot-mode)
;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
