;; Package Management
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Path management
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

;; Startup time
(setq use-package-compute-statistics t)

(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

;; Ignore files in neotree
(setq neo-hidden-regexp-list
    '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" ;; defaults
      ;; add yours:
      "node_modules"))

;; all-the-icons
(use-package all-the-icons
  :ensure t)

;; all-the-icons-dired
;; (use-package all-the-icons-dired
  ;; :ensure t)

;; Use icons in neotree
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-theme 'icons)

;; Use icons in dired mode
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)



;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(use-package flycheck
  :ensure t
  ;; :init (global-flycheck-mode)
  :commands flycheck-mode
  :init
    (add-hook 'js2-mode-hook 'flycheck-mode))

;; org load languages
;; (org-babel-do-load-languages 'org-babel-load-languages
                            ;; '((shell . t)
                            ;; (lisp . t)))

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

;; Powerline
(use-package powerline
  :ensure t)
(powerline-default-theme)
(use-package airline-themes
  :ensure t)
(load-theme 'airline-dark t)

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :commands company-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'js2-mode-hook 'company-mode)
  (add-hook 'web-mode-hook 'company-mode)
  (add-hook 'css-mode-hook 'company-mode)
  (add-hook 'org-mode-hook 'company-mode))

;; Treesitter
(use-package  tree-sitter
  :ensure t
  :commands tree-sitter-hl-mode)
(use-package tree-sitter-langs
  :ensure t
  :commands tree-sitter-hl-mode)

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

;; Javascript
(use-package js2-mode 
  :ensure t
  :mode "\\.js\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)
  (add-hook 'js2-mode-hook 'tree-sitter-hl-mode)))

;; Typescript
(use-package typescript-mode
  :mode "\\.tsx\\'"
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'tree-sitter-hl-mode))

;; Web mode
(use-package web-mode
  :ensure t
  :hook (web-mode . lsp-deferred))
(setq web-mode-markup-indent-offset 2)

;; Svelte mode
(use-package svelte-mode
  :ensure t
  :mode "\\.svelte\\'"
  :init
  (add-hook 'svelte-mode-hook 'tree-sitter-hl-mode))

;; LSP
(use-package lsp-mode
  :ensure t
   :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (js-mode . lsp-deferred)
	 (html-mode . lsp-deferred)
	 (css-mode . lsp-deferred)
   (typescript-mode . lsp-deferred)
   (svelte-mode . lsp-deferred))
  :commands lsp-deferred)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; LSP performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))


(use-package add-node-modules-path
  :ensure t)

(use-package prettier-js
  :ensure t
  :init
  (require 'prettier-js)
  (add-hook 'js2-mode-hook 'add-node-modules-path)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'add-node-modules-path)
  (add-hook 'web-mode-hook 'prettier-js-mode))


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
(use-package counsel
  :ensure t)

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

;; Project management
(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recently-active)

  ; Route errors to /dev/null
  (setq projectile-git-submodule-command "git submodule --quiet foreach 'echo $path' 2>/dev/null | tr '\\n' '\\0'")
  :config
  (projectile-mode))

(use-package counsel-projectile 
  :ensure t
  :config
  (counsel-projectile-mode))

;; Window Layouts with Perspective
(use-package perspective
  :ensure t
  :config
  (persp-mode))
(use-package persp-projectile
  :ensure t)

;; Surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)
(setq esup-depth 0)

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

(use-package perspective
  :demand t
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))


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
                 (visual-line-mode 1)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)
                 (visual-fill-column-mode 0)
                 (visual-line-mode 0)))))

;; For reading epub files 
(use-package nov
  :ensure t
  :mode "\\.nov\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)

