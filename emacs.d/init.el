;; -*- lexical-binding: t; -*-

;;; package --- my init.el

;; Load common settings
(load (concat user-emacs-directory "rajasegar/settings.el"))

;; Load package config
(load (concat user-emacs-directory "rajasegar/packages.el"))

;; Load tab line config
(load (concat user-emacs-directory "rajasegar/tab-line-config.el"))

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

;; Load dashboard footer messages
(load (concat user-emacs-directory "rajasegar/dashboard-footer-messages.el"))

;; Load prodigy services
(load (concat user-emacs-directory "rajasegar/prodigy-services.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(ellama-blueprints
   '((:act "Warren Buffett Stock Analysis" :prompt
           "---\12name: warren-buffett-stock-analysis\12description: Research Indian stocks and companies based on the approaches and methodologies used by the renowned investor Warren Buffett. \12---\12\12## Warren Buffett Stock Analysis Skill\12Frameworks and methodologies for researching stocks, comparing , and identifying stock market buying opportunities.\12\12\12### Business Tenets\12- Is the business simple and understandable?\12- Does the business have a consistent operating history?\12- Does the business have favorable long-term prospects?\12\12### Management Tenets\12- Is management rational?\12- Is management candid with its shareholders?\12- Does the management resist the institutional imperative?\12\12### Financial Tenets\12- Focus on RoE not EPS\12- Calculate “owner earnings”\12- Look for companies with high profit margins\12- For every dollar retained, make sure the company has created at least one dollar of market value\12\12### Market Tenets\12- What is the value of the business?\12- Can the business be purchased at a significant discount to its value?\12\12"
           :for-devs nil)))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(add-node-modules-path copilot corfu dashboard deflate doom-modeline
                           doom-themes emmet-mode emms evil-collection
                           exec-path-from-shell git-gutter
                           git-timemachine gptel json-mode magit
                           markdown-mode mcp minuet org-bullets
                           plz-event-source prettier-js prodigy slime
                           spinner tree-sitter-langs treemacs-evil
                           visual-fill-column web-mode yaml-mode))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch
              "main")))
 '(safe-local-variable-values '((lsp-enabled-clients quote (bun-lsp-server))))
 '(web-mode-css-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
