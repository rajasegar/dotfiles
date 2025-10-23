;;; packages.el --- This is the package configuration file
;; Package Management

;;; Commentary:
;; All package configuration goes here

(require 'package)
;;; Code:

(setq package-enable-at-startup nil)
(setq package-archives
      '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Startup time
(setq use-package-compute-statistics t)

;; Path management
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package nerd-icons
  :ensure t
  )

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-compute-remaps t)
  :config
  (which-key-mode))

(setq org-agenda-files (list "~/Dropbox/org/rajasegar.org"
                             "~/Dropbox/org/birthdays.org"
                             "~/Dropbox/org/anniversaries.org"))
(setq org-default-notes-file "~/Dropbox/org/tasks.org")


;; Org mode enhancements
(use-package org-bullets
  :ensure t
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode))

;; Org-agenda customizations
(setq org-agenda-start-on-weekday 0)
(setq org-agenda-timegrid-use-ampm 1)

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))


;; Treesitter install only for linux
(when (string-equal system-type  "gnu/linux")
  (use-package  tree-sitter
    :ensure t
    :commands (tree-sitter-hl-mode)
    )
  (use-package tree-sitter-langs
    :ensure t
    :commands (tree-sitter-hl-mode)
    )
  )

(setq treesit-language-source-alist
      '((tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
                    "v0.20.3"
                    "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "v0.20.3"
                    "typescript/src")))
;; Enable Tree sitter modes
(add-hook 'rjsx-mode-hook 'tree-sitter-hl-mode)
(add-hook 'typescript-mode-hook 'tree-sitter-hl-mode)
(add-hook 'js-mode-hook 'tree-sitter-hl-mode)

;; yaml
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; json
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))


;; Web mode
(use-package web-mode
  :ensure t)

(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

(use-package add-node-modules-path
  :ensure t)

(use-package prettier-js
  :ensure t
  :init
  (require 'prettier-js)
  (add-hook 'js-mode-hook 'add-node-modules-path)
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'add-node-modules-path)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(setq prettier-js-args '(
  "--single-quote" "true"
  "--trailing-comma" "none"
))

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Magit
(use-package magit
  :ensure t
  :commands (magit-mode))

;; Git gutter
(use-package git-gutter :ensure t)
(global-git-gutter-mode +1)

;; Startup screen with dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)
                        (bookmarks . 5)))
(setq dashboard-center-content t)
(setq dashboard-display-icons-p nil) ;; display icons on both GUI and terminal
;; Use project.el for  projects
(setq dashboard-projects-backend 'project-el)

;; emmet-mode
(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))


(use-package visual-fill-column
  :ensure t)

;; Configure fill width
(setq visual-fill-column-width 110)
(setq-default visual-fill-column-center-text t)

(use-package prodigy
  :ensure t)


(setq flymake-no-changes-timeout 0.5)

;; Treemacs
(use-package treemacs
  :defer t
  :config
  (treemacs-hide-gitignored-files-mode))

;; hide node modules in treemacs
(with-eval-after-load 'treemacs
  (defun treemacs-ignore-example (filename absolute-path)
    (or (string-equal filename "node_modules")
        (string-prefix-p "/x/y/z/" absolute-path)))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-example))

(use-package treemacs-evil
  :ensure t
  :after treemacs evil)

(setq highlight-indent-guides-method  'character)
(setq  highlight-indent-guides-character ?¦)

  
;; Themes
 (use-package doom-themes
   :ensure t
   :config
   (load-theme 'doom-dracula t))

;; Modeline
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

;; (setq doom-modeline-workspace-name t)

;; Vim mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq-default evil-escape-delay 0.2))

;; evil-collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package gimp
  :load-path "elpa/gimp.el")

(use-package shell-command-queue
  :load-path "elpa/shell-command-queue")


(provide 'packages)

;;; packages.el ends here

