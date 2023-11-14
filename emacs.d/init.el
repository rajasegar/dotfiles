;; -*- lexical-binding: t; -*-

;;; package --- my init.el

;; Load common settings
(load (concat user-emacs-directory "rajasegar/settings.el"))

;; Load package config
(load (concat user-emacs-directory "rajasegar/packages.el"))

;; Load emacs-lsp-mode config
;; (load (concat user-emacs-directory "rajasegar/emacs-lsp-mode.el"))

;; Load eglot config
(load (concat user-emacs-directory "rajasegar/eglot-config.el"))

;; Load music config
(load (concat user-emacs-directory "rajasegar/music.el"))

;; Load user defined functions
(load (concat user-emacs-directory "rajasegar/functions.el"))

;; Load keybindings
(load (concat user-emacs-directory "rajasegar/keybindings.el"))

;; Load copilot
;; (load (concat user-emacs-directory "rajasegar/copilot.el"))

;; Load codeium
(load (concat user-emacs-directory "rajasegar/codeium.el"))

;; Load dashboard footer messages
(load (concat user-emacs-directory "rajasegar/dashboard-footer-messages.el"))

;; Load prodigy services
(load (concat user-emacs-directory "rajasegar/prodigy-services.el"))

;; Load centaur tabs
(load (concat user-emacs-directory "rajasegar/centaur-tabs.el"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(codeium/metadata/api_key "55c40452-3c40-46a6-bdbb-8610ea1e13d5")
 '(css-indent-offset 2)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(nerd-icons-dired diredfl prodigy doom-modeline git-timemachine nova-theme svelte-mode visual-fill-column org-present lsp-ui yasnippet org-tempo all-the-icons-dired nov typescript-mode neotree airline-themes linum-relative olivetti which-key use-package ranger prettier-js multi-term js2-mode general exec-path-from-shell evil doom-themes counsel-projectile))
 '(safe-local-variable-values '((lsp-enabled-clients quote (bun-lsp-server))))
 '(web-mode-css-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
