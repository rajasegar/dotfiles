;; Keybindings
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
   "/"   'counsel-rg
   "SPC"   'counsel-M-x
   "."   'edit-emacs-configuration
   "\""  'split-window-below
   "%"  'split-window-right
   "TAB" 'toggle-buffers
   
   "T" 'counsel-load-theme

   "a RET" 'emms-playlist-mode-go
   "a SPC" 'emms-pause
   "a e" 'emms
   "a" '(:ignore t :which-key "Applications")
   "am" '(:ignore t :which-key "Music")
   "amb" 'emms-browser
   "amd" 'emms-play-directory
   "amn" 'emms-next
   "amo" 'emms-show
   "amp" 'emms-previous
   "ams" 'emms-shuffle
   "am1" 'play-favs-folder
   "am2" 'play-college-folder
   "am3" 'play-college-folder
   "ar" 'elfeed
   "at" 'open-new-eshell

   "ap" '(:ignore t :which-key "Org Present")
   "apx" 'org-present
   "apn" 'org-present-next
   "app" 'org-present-prev

   "b" '(:ignore t :which-key "Buffers")
   "bb"  'ivy-switch-buffer
   "bd" 'kill-this-buffer
   "be" 'eval-buffer
   "bh" 'switch-to-dashboard
   "bs" 'scratch-buffer

   "c" '(:ignore t :which-key "Comment")
   "cl" 'comment-line

   "d" '(:ignore t :which-key "Comments")
   "df" 'js-doc-insert-function-doc
   "dm" 'js-doc-insert-file-doc

   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fs" 'save-buffer
   "fe" '(:ignore t :which-key "Edit Configs")
   "fef" 'rajasegar/edit-emacs-functions
   "fek" 'rajasegar/edit-emacs-keybindings
   "fep" 'rajasegar/edit-emacs-packages
   "fes" 'rajasegar/edit-emacs-settings

   "g" '(:ignore t :which-key "Code?")
   "gh" 'switch-git-personal
   "gs" 'magit-status
   "gw" 'switch-git-work

   "gi" 'lsp-goto-implementation
   "gt" 'lsp-goto-type-definition
   "gd" 'lsp-find-definition

   "l" '(:ignore t :whick-key "Perspective")
   "lc" 'persp-new
   "ll" 'persp-switch
   "ln" 'persp-next
   "lp" 'persp-prev
   "lk" 'persp-kill
   

   "o" '(:ignore t : which-key "Org-Mode")
   "oa" 'org-agenda
   "oc" 'org-capture
   "ot" 'org-toggle-link-display

   "p" 'projectile-command-map
   "pf" 'counsel-projectile-find-file
   "pp" 'projectile-persp-switch-project
   "pt" 'neotree-project-dir

   "qq" '(kill-emacs)
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
   :states '(visual)
   "gc" 'evilnc-comment-or-uncomment-lines))
