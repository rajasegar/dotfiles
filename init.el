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
  :config
  (evil-mode 1)
  (setq-default evil-escape-delay 0.2))

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
  :ensure t)
(global-set-key [f8] 'neotree-toggle)

(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "j") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "k") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

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
  :ensure t)

;; LSP
(use-package lsp-mode
  :ensure t
  :hook (
	 (js2-mode . lsp-deferred)
	 (web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)

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
  (add-hook 'web-mode-hook 'add-node-modules-path))

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
(emms-play-directory "~/Music/Favs")
(emms-random)


; Edit this config
(defun edit-emacs-configuration ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun toggle-buffers ()
  (interactive)
  (switch-to-buffer nil))

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
   :states '(normal visual emacs)
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

   "p" 'projectile-command-map
   "pp" 'projectile-persp-switch-project
   "pf" 'counsel-projectile-find-file
   "pt" 'neotree-project-dir

   "b" '(:ignore t :which-key "Buffers")
   "bb"  'ivy-switch-buffer
   "bd" 'kill-this-buffer
   "be" 'eval-buffer

   "w" '(:ignore t :which-key "Window")
   "wl"  'windmove-right
   "wh"  'windmove-left
   "wk"  'windmove-up
   "wj"  'windmove-down
   "w-"  'split-window-below
   "w/"  'split-window-right
   "wd"  'delete-window
   "ww"  'evil-window-next

   "a" '(:ignore t :which-key "Applications")
   "at" 'eshell

   "s" '(:ignore t :which-key "Search")
   "sc" 'evil-ex-nohighlight
   "sl" 'ivy-resume

   "t" '(:ignore t :which-key "Toggles")
   "tn" 'display-line-numbers-mode
   "tl" 'toggle-truncate-lines

   "T" 'counsel-load-theme
   
   "x" '(:ignore t :which-key "Text")
   "xl" '(:ignore t :which-key "Lines")
   "xls" 'sort-lines
   
   "g" '(:ignore t :which-key "Code?")
   "gc" 'evilnc-comment-or-uncomment-lines
   "gs" 'magit-status

   "d" '(:ignore t :which-key "Comments")
   "df" 'js-doc-insert-function-doc
   "dm" 'js-doc-insert-file-doc

   "fs" 'save-buffer
   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fer" '(load user-init-file)

   "qr" '(kill-emacs 123)
   "qq" '(kill-emacs)

   "am" '(:ignore t :which-key "Music")
   "amd" 'emms-play-directory
   "amb" 'emms-browser
   "amo" 'emms-show
   "amn" 'emms-next
   "amp" 'emms-previous
   "a SPC" 'emms-pause
   "a RET" 'emms-smart-browse
   "a e" 'emms
   "ar" 'emms-random
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
