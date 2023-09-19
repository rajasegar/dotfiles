;; Keybindings

(global-set-key [f5] 'emms-start)
(global-set-key [f6] 'emms-stop)
(global-set-key [f7] 'emms-previous)
(global-set-key [f8] 'emms-next)
(global-set-key [f9] 'neotree-show)
(global-set-key [f12] 'persp-next)

(use-package general
  :ensure t
  :config 
  (general-define-key
   "M-x" 'counsel-M-x)

  (general-define-key
   :states '(normal visual)
   "C-u" 'scroll-down-command
   "C-d" 'scroll-up-command)

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "'"   'multi-term
   "/"   'counsel-git-grep
   "SPC"   'counsel-M-x
   "."   'edit-emacs-configuration
   ";"   'emms-pause
   "\""  'split-window-below
   "%"  'split-window-right
   "TAB" 'toggle-buffers
   
   "T" 'counsel-load-theme

   "a" '(:ignore t :which-key "Applications")
   "ac" 'calendar
   "ad" 'dired
   "ae" 'emms
   "ah" 'rajasegar/open-hackernews
   "ap" 'org-present
   "ar" 'elfeed
   "at" 'open-new-eshell

   "b" '(:ignore t :which-key "Buffers")
   "bb"  'ivy-switch-buffer
   "bd" 'kill-this-buffer
   "be" 'eval-buffer
   "bh" 'switch-to-dashboard
   "bn" 'centaur-tabs-forward
   "bp" 'centaur-tabs-backward
   "bs" 'scratch-buffer

   "c" '(:ignore t :which-key "Comment")
   "cl" 'comment-line

   "d" '(:ignore t :which-key "Comments")
   "df" 'js-doc-insert-function-doc
   "dm" 'js-doc-insert-file-doc

   "e" '(:ignore t :which-key "Edit Configs")
   "ef" 'rajasegar/edit-emacs-functions
   "ek" 'rajasegar/edit-emacs-keybindings
   "ep" 'rajasegar/edit-emacs-packages
   "es" 'rajasegar/edit-emacs-settings

   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fs" 'save-buffer

   "g" '(:ignore t :which-key "Git")
   "gb" 'magit-blame
   "gh" 'switch-git-personal
   "gs" 'magit-status
   "gt" 'git-timemachine
   "gw" 'switch-git-work

   "h" '(:ignore t :which-key "Help")
   "hb" 'counsel-descbinds
   "hf" 'counsel-describe-function
   "hv" 'counsel-describe-variable

   "l" '(:ignore t :whick-key "Perspective")
   "lc" 'persp-new
   "ll" 'persp-switch
   "ln" 'persp-next
   "lp" 'persp-prev
   "lk" 'persp-kill

   "m" '(:ignore t :which-key "Music")
   "mb" 'emms-browser
   "md" 'emms-play-directory-tree
   "mn" 'emms-next
   "mo" 'emms-show
   "mp" 'emms-previous
   "ms" 'emms-shuffle
   "mx" 'emms-stop
   "m1" 'rajasegar/play-favs-folder
   "m2" 'rajasegar/play-college-folder
   "m3" 'rajasegar/play-latest-folder
  

   "o" '(:ignore t : which-key "Org-Mode")
   "oa" 'org-agenda
   "oc" 'org-capture
   "ot" 'org-toggle-link-display
   "os" 'org-timer-set-timer
   "ox" 'org-timer-stop

   "p" 'projectile-command-map
   "pf" 'counsel-projectile-find-file
   "pp" 'projectile-persp-switch-project
   "pt" 'neotree-show

   "qq" 'kill-emacs
   "qr" 'restart-emacs

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

   "y" '(:ignore t :which-key "Yasnippet")
   "yn" 'yas-new-snippet
   "yi" 'yas-insert-snippet)

  (general-define-key
   :states '(normal visual)
   :keymaps 'dired-mode-map
   "p" 'emms-play-dired)

  (general-define-key
 :states '(normal visual)
 :keymaps 'typescript-mode-map
   "gi" 'lsp-goto-implementation
   "gt" 'lsp-goto-type-definition
   "gd" 'lsp-find-definition)

  (general-define-key
   :states '(visual)
   "gc" 'evilnc-comment-or-uncomment-lines)

  (general-define-key
   :states '(normal visual)
   :keymaps 'neotree-mode-map
   "o" 'neotree-quick-look))
