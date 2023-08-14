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

;; don't create lock files
(setq create-lockfiles nil)

;; start in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq js-indent-level 2)

;; Eshell case insensitive glob
(setq eshell-cmpl-ignore-case t)


;; Enable copy paste
(setq select-enable-clipboard t)

(global-set-key (kbd "C-S-v") #'clipboard-yank)

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

;; Highlight current line
(global-hl-line-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook
                org-present-mode-hook
                dashboard-mode-hook
                nov-mode-hook
                eww-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Font
(when (string-equal system-type "darwin")
    (add-to-list 'default-frame-alist '(font . "Monaco-16" ))
    (set-face-attribute 'default t :font "Monaco-16" ))

(when (string-equal system-type  "gnu/linux")
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11" ))
    (set-face-attribute 'default t :font "DejaVu Sans Mono-11" ))

(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

(setq org-clock-sound "~/.emacs.d/bell.wav")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/rajasegar.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("e" "Emacs Todo" entry (file+headline "~/Dropbox/org/rajasegar.org" "Emacs")
         "* TODO %?\n  %i\n  %a")
        ("f" "Freshchat Todo" entry (file+headline "~/Dropbox/org/freshdesk.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq nov-text-width t)
(setq visual-fill-column-center-text t)


;; Enable visual line and fill column for some modes
(dolist (mode '(nov-mode-hook
                eww-mode-hook))
  (add-hook mode (lambda ()
                   (visual-line-mode 1)
                   (visual-fill-column-mode 1))))

(setq lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
