;;; packages.el --- This is the package configuration file
;; Package Management

;;; Commentary:
;; All package configuration goes here

(require 'package)
;;; Code:

(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
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



;; Themes
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))

;; Neotree
(use-package neotree
  :ensure t
  :config
  ;; Disable line-numbers minor mode for neotree
  (add-hook 'neo-after-create-hook
            (lambda (&rest _) (display-line-numbers-mode -1))))
(setq neo-smart-open t)

;; Ignore files in neotree
(setq neo-hidden-regexp-list
    '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" ;; defaults
      ;; add yours:
      "node_modules"))

;; all-the-icons
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Use icons in neotree
(setq neo-theme 'icons)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(setq org-agenda-files (list "~/Dropbox/org/freshdesk.org"
                             "~/Dropbox/org/rajasegar.org"
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

;; Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-workspace-name t)


(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq-default
   company-idle-delay 0.05
   company-minimum-prefix-length 0
   company-require-match nil))

;; Disable company mode for specific modes
(setq company-global-modes '(not org-mode eshell-mode))



;; Treesitter install only for linux
(when (string-equal system-type  "gnu/linux")
  (use-package  tree-sitter
    :ensure t
    :commands (tree-sitter-hl-mode))
  (use-package tree-sitter-langs
    :ensure t
    :commands (tree-sitter-hl-mode)))
(add-hook 'prog-mode-hook 'tree-sitter-hl-mode)

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


;; Typescript
(use-package typescript-mode
  :mode "\\.tsx\\'"
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'tree-sitter-hl-mode))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; Web mode
(use-package web-mode
  :ensure t)

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

;; Slime
(use-package slime
  :ensure t
  :commands (slime-mode))
(setq inferior-lisp-program "sbcl")

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Ivy & friends
(use-package ivy
  :ensure t)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(use-package counsel
  :ensure t)
(counsel-mode)

;; Magit
(use-package magit
  :ensure t
  :commands (magit-mode))
;(use-package git-gutter :ensure t)
;(global-git-gutter-mode +1)

;; Git timemachine
(use-package git-timemachine
  :ensure t
  :commands (git-timemachine))

;; Startup screen with dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))
(setq dashboard-center-content t)
(setq dashboard-display-icons-p nil) ;; display icons on both GUI and terminal
;; Use project.el for  projects
(setq dashboard-projects-backend 'project-el)

;; plantuml
(use-package plantuml-mode
  :mode "\\.pum\\'"
  :ensure t)

;; Sample jar configuration
(setq plantuml-jar-path "~/plantuml.jar")
(setq org-plantuml-jar-path "~/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

;; Enable plantuml for org-mode
;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

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

(use-package org-present
  :ensure t)

(setq org-image-actual-width nil)

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)
                 (visual-fill-column-mode 1)
                 (visual-line-mode 1)
                 (evil-mode 0)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)
                 (visual-fill-column-mode 0)
                 (visual-line-mode 0)
                 (evil-mode 1)))))

;; yasnippet
(use-package yasnippet
  :ensure t)
(yas-global-mode 1)

(use-package yasnippet-snippets
  :ensure t)

(use-package prodigy
  :ensure t)

;; Colorful dired
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package flymake-eslint
  :hook
  (eglot-managed-mode . (lambda ()
                          (when (derived-mode-p 'typescript-mode 'js-mode)
                            (flymake-eslint-enable)))))

(use-package eshell-extensions
  :load-path "elpa/eshell-extensions/")

(setq highlight-indent-guides-method  'character)
(setq  highlight-indent-guides-character ?¦)

(use-package highlight-indent-guides
  :ensure t
  ;; git clone https://github.com/Dickby/highlight-indent-guides
  :load-path "elpa/highlight-indent-guides"
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  )

(provide 'packages)

;;; packages.el ends here
