;;; package --- my init.el

;;; Commentary:
;; Got this config from this url and tweaking for my taste
;; http://www.petecorey.com/blog/2019/07/01/building-my-own-spacemacs/
;; Helpful links:
;; 
;; - https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; - https://dev.to/huytd/emacs-from-scratch-1cg6
;; - https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/#fnref:2

;;; Code:
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq delete-old-versions -1 )
(setq inhibit-startup-screen t )
(setq ring-bell-function 'ignore )
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq initial-scratch-message "")
(setq word-wrap t)

;; Tab settings
(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)

;; Eshell case insensitive glob
(setq eshell-cmpl-ignore-case t)

;; Enable copy paste
(setq x-select-enable-clipboard t)

;; https://github.com/danielmai/.emacs.d/blob/master/config.org
(defalias 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode -1)
(show-paren-mode t)

(setq-default mode-line-format nil)

;; (toggle-word-wrap)
(global-auto-revert-mode t)
(global-display-line-numbers-mode 1)
(electric-pair-mode)

;; Set relative line numbers
(setq display-line-numbers-type 'relative)

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

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Font
(add-to-list 'default-frame-alist '(font . "Monaco-14" ))
(set-face-attribute 'default t :font "Monaco-14" )

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

(global-set-key [f8] 'neotree-toggle)

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

(global-set-key [f8] 'neotree-project-dir)

;; Ignore files in neotree
(setq neo-hidden-regexp-list
    '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" ;; defaults
      ;; add yours:
      "node_modules"))


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
  :init (global-flycheck-mode))

;; org load languages
(org-babel-do-load-languages 'org-babel-load-languages
    '((shell . t)))

(setq org-agenda-files (list "~/Dropbox/org/freshdesk.org" "~/Dropbox/org/rajasegar.org"))
(setq org-default-notes-file "~/Dropbox/org/tasks.org")

;; Org mode enhancements
(use-package org-bullets
  :ensure t
  :init (org-mode))

;; Powerline
(use-package powerline
  :ensure t)
(powerline-default-theme)
(use-package airline-themes
  :ensure t)
(load-theme 'airline-dark t)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))
(setq company-idle-delay 0.0)

;; yaml
(use-package yaml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; json
(use-package json-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

;; Skewer
(use-package simple-httpd
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package skewer-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
  
;; Javascript
(use-package js2-mode 
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

;; Web mode
(use-package web-mode
  :ensure t
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

;; LSP
(use-package lsp-mode
  :ensure t
  :hook (
	 (js2-mode . lsp-deferred)
	 (web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)
;; LSP performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb


;; TSX
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; JSX
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

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
  :ensure t)
(setq inferior-lisp-program "sbcl")
;; Mac OSX owns C-up and C-down, so arrange for history
;; navigation to do something useful via C-c p and C-c n.
(eval-after-load 'slime
  `(progn
     (define-key slime-prefix-map "p" 'slime-repl-backward-input)
     (define-key slime-prefix-map "n" 'slime-reply-forward-input)))

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


;; Code commenting
(use-package evil-nerd-commenter :ensure t)

;; Magit
(use-package magit :ensure t)
(use-package git-gutter :ensure t)
(global-git-gutter-mode +1)

;; Startup screen with dashboard
(use-package page-break-lines
  :ensure t)
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

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

;; Workspaces
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

;; EMMS
(use-package emms
  :ensure t)
(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'emms-setup)
(require 'emms-player-mplayer)
(emms-standard)
(emms-default-players)
(define-emms-simple-player mplayer '(file url)
  (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
  "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")
(require 'emms-history)
(emms-history-load)

;; elfeed
(use-package elfeed
  :ensure t)

(setq elfeed-feeds
      '("https://css-tricks.com/feed/"
	"https://dev.to/feed"
	"https://hnrss.org/frontpage"
	"https://www.smashingmagazine.com/feed"
	"https://alistapart.com/main/feed/"
	))

;; plantuml
(use-package plantuml-mode
  :ensure t)
;; Sample jar configuration
(setq plantuml-jar-path "~/plantuml.jar")
(setq org-plantuml-jar-path "~/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
;; Enable plantuml for org-mode
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

;; emmet-mode
(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package perspective
  :demand t
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

; Edit this config
(defun edit-emacs-configuration ()
  "Edit Emacs configuration."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun toggle-buffers ()
  "Toggle buffers."
  (interactive)
  (switch-to-buffer nil))

(defun switch-to-dashboard ()
 "Switch to dashboard buffer."
  (interactive)
  (switch-to-buffer dashboard-buffer-name))

(defun switch-git-personal ()
  "Switch to personal Github profile."
  (interactive)
  (shell-command "ssh-add -D && ssh-add ~/.ssh/id_rsa && ssh -T git@github.com"))

(defun switch-git-work ()
  "Switch to work Github profile."
  (interactive)
  (shell-command "ssh-add -D && ssh-add ~/.ssh/id_ed25519 && ssh -T git@github.com"))

(defun open-new-eshell ()
  "Open new eshell instance everytime"
  (interactive)
  (eshell 'N))

;; Keybindings
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "Kj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "KJ" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kJ" 'evil-normal-state))
(use-package general
  :ensure t
  :config 
  (general-define-key
   "M-x" 'counsel-M-x)
  (general-define-key
   :states '(normal visual)
   ;; "/" 'swiper
   "gcc" 'evilnc-comment-or-uncomment-lines)
  (general-define-key
   :states '(normal visual)
   "C-u" 'scroll-down-command
   "C-d" 'scroll-up-command)
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "'"   'multi-term
   "/"   'counsel-rg
   "SPC" 'counsel-M-x
   "."   'edit-emacs-configuration
   "\""  'split-window-below
   "%"   'split-window-right
   "TAB" 'toggle-buffers
   
   "T" 'counsel-load-theme

   "a RET" 'emms-smart-browse
   "a SPC" 'emms-pause
   "a e" 'emms
   "a" '(:ignore t :which-key "Applications")
   "am" '(:ignore t :which-key "Music")
   "amb" 'emms-browser
   "amd" 'emms-play-directory
   "amn" 'emms-next
   "amo" 'emms-show
   "amp" 'emms-previous
   "amr" 'emms-random
   "ar" 'elfeed
   "at" 'open-new-eshell

   "b" '(:ignore t :which-key "Buffers")
   "bb"  'ivy-switch-buffer
   "bd" 'kill-this-buffer
   "be" 'eval-buffer
   "bh" 'switch-to-dashboard

   "c" '(:ignore t :which-key "Comment")
   "cl" 'comment-line

   "d" '(:ignore t :which-key "Comments")
   "df" 'js-doc-insert-function-doc
   "dm" 'js-doc-insert-file-doc

   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fs" 'save-buffer

   "g" '(:ignore t :which-key "Code?")
   "gc" 'evilnc-comment-or-uncomment-lines
   "gh" 'switch-git-personal
   "gs" 'magit-status
   "gw" 'switch-git-work
   
   "l" '(:ignore t :whick-key "Perspective")
   "ll" 'persp-switch
   "ln" 'persp-next
   "lp" 'persp-prev

   "o" '(:ignore t : which-key "Org-Mode")
   "oa" 'org-agenda
   "oc" 'org-capture
   "ot" 'org-toggle-link-display

   "p" 'projectile-command-map
   "pf" 'counsel-projectile-find-file
   "pp" 'projectile-persp-switch-project
   "pt" 'neotree-project-dir

   "qq" '(kill-emacs)
   "qr" '(kill-emacs 123)

   "s" '(:ignore t :which-key "Slime")
   "ss" 'slime
   "sl" 'slime-load-file

   "t" '(:ignore t :which-key "Toggles")
   "tl" 'toggle-truncate-lines
   "tn" 'display-line-numbers-mode

   "w" '(:ignore t :which-key "Window")
   "w-"  'split-window-below
   "w/"  'split-window-right
   "wd"  'delete-window
   "wh"  'windmove-left
   "wj"  'windmove-down
   "wk"  'windmove-up
   "wl"  'windmove-right
   "ww"  'evil-window-next

   "x" '(:ignore t :which-key "Text")
   "xl" '(:ignore t :which-key "Lines")
   "xls" 'sort-lines
   )
  (general-define-key
   :states '(visual)
   "gc" 'evilnc-comment-or-uncomment-lines))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(airline-themes linum-relative olivetti which-key use-package ranger prettier-js multi-term js2-mode general exec-path-from-shell evil doom-themes counsel-projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
