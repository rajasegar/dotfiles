;;; package --- my init.el

;;; Commentary:
;; Got this config from this url and tweaking for my taste
;; http://www.petecorey.com/blog/2019/07/01/building-my-own-spacemacs/
;; Helpful links:
;; 
;; - https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; - https://dev.to/huytd/emacs-from-scratch-1cg6
;; - https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/#fnref:2

;; Load common settings
(load (concat user-emacs-directory "rajasegar/settings.el"))

;; Load package config
(load (concat user-emacs-directory "rajasegar/packages.el"))

;; Load music config
(load (concat user-emacs-directory "rajasegar/music.el"))

;; Load user defined functions
(load (concat user-emacs-directory "rajasegar/functions.el"))

;; Load keybindings
(load (concat user-emacs-directory "rajasegar/keybindings.el"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(svelte-mode visual-fill-column org-present lsp-ui yasnippet org-tempo all-the-icons-dired nov typescript-mode neotree airline-themes linum-relative olivetti which-key use-package ranger prettier-js multi-term js2-mode general exec-path-from-shell evil doom-themes counsel-projectile))
 '(web-mode-css-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
