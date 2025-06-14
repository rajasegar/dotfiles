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

;; all-the-icons
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-compute-remaps t)
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

;; Magit
(use-package magit
  :ensure t
  :commands (magit-mode))

;; Git gutter
(use-package git-gutter :ensure t)
(global-git-gutter-mode +1)

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
(setq plantuml-default-exec-mode 'jar)

;; (setq plantuml-jar-args '("-tpng"))
;; (setq plantuml-output-type "png")
;; (setq plantuml-java-args (list "-Djava.awt.headless=true" "-jar"))
;; (add-to-list 'auto-mode-alist '("\\.pum\\'" . plantuml-mode))

;; Enable plantuml for org-mode
;; (setq org-plantuml-jar-path "~/plantuml.jar")
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

(use-package prodigy
  :ensure t)


(setq flymake-no-changes-timeout 0.5)

(use-package eshell-extensions
  :load-path "elpa/eshell-extensions/")

(use-package freshrelease
  :load-path "elpa/freshrelease/")

(use-package github
  :load-path "elpa/github")


(use-package multiple-cursors
  :ensure t)


;; (use-package ipl
  ;; :load-path "elpa/ipl")


;; For reading epub files 
;; (use-package nov
  ;; :ensure t
  ;; :mode "\\.nov\\'"
  ;; :init
  ;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
;; (setq nov-text-width t)
;; (setq visual-fill-column-center-text t)
;; (add-hook 'nov-mode-hook 'visual-line-mode)
;; (add-hook 'nov-mode-hook 'visual-fill-column-mode)

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

(use-package react-migration
  :load-path "elpa/react-migration")

;; (use-package glimmer-ast
  ;; :load-path "elpa/glimmer-ast")

;; (use-package freddy-ai
  ;; :load-path "elpa/freddy-ai")

(use-package llm
  :ensure t)


(use-package ellama
  :ensure t
  :init
  (setopt ellama-keymap-prefix "C-x c")
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
		      (make-llm-ollama
		       :chat-model "llama3.2" :embedding-model "llama3.2"))
  (setopt ellama-providers
		      '(("tiny" . (make-llm-ollama
				               :chat-model "tinyllama"
				               :embedding-model "tinyllama"))
		        ("code" . (make-llm-ollama
				               :chat-model "codellama"
				               :embedding-model "codellama"))
		        ("llama" . (make-llm-ollama
				                :chat-model "llama3.2"
				                :embedding-model "llama3.2"))
            ("deepseek-coder" . (make-llm-ollama
				                        :chat-model "deepseek-coder"
				                        :embedding-model "deepseek-coder"))
            ("gemma2" . (make-llm-ollama
				                        :chat-model "gemma2"
				                        :embedding-model "gemma2"))
            ("phi3" . (make-llm-ollama
                       :chat-model "phi3"
                       :embedding-model "phi3"))
            )
          )
  )
  
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

(use-package evil-surround
  :load-path "elpa/evil-surround/"
  :config
  (global-evil-surround-mode 1))

(provide 'packages)

;;; packages.el ends here

