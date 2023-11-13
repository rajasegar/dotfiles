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

;; Startup time
(setq use-package-compute-statistics t)

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
(setq neo-smart-open t)

;; Ignore files in neotree
(setq neo-hidden-regexp-list
    '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" ;; defaults
      ;; add yours:
      "node_modules"))

;; all-the-icons
(use-package all-the-icons
  :ensure t)


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

(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq-default
   company-idle-delay 0.05
   company-minimum-prefix-length 0))



;; Treesitter install only for linux
(when (string-equal system-type  "gnu/linux")
  (use-package  tree-sitter
    :ensure t
    :commands (tree-sitter-hl-mode))
  (use-package tree-sitter-langs
    :ensure t
    :commands (tree-sitter-hl-mode)))

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
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'tree-sitter-hl-mode))

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
  (add-hook 'js2-mode-hook 'add-node-modules-path)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
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


;; Tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (setq
   centaur-tabs-set-icons t
   centaur-tabs-gray-out-icons 'buffer
   centaur-tabs-set-bar 'left
   centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  :init
  (setq centaur-tabs-enable-key-bindings t))

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*scratch" name)
     (string-prefix-p "*Messages" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*company" name)
     ;; (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*dashboard" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "magit-" name)
     (string-prefix-p "magit:" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

;; yasnippet
(use-package yasnippet
  :ensure t)
(yas-global-mode 1)

(use-package yasnippet-snippets
  :ensure t)

(use-package prodigy
  :ensure t)

