;;; keybindings.el --- Rajasegar Chandran


;;; Commentary:
;; All my custom keybindings are defined here

;;; Code:

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
   "/"   'counsel-rg
   "SPC"   'counsel-M-x
   "."   'rajasegar/edit-emacs-configuration
   ";"   'emms-pause
   "\""  'split-window-below
   "%"  'query-replace
   "TAB" 'rajasegar/toggle-buffers
   
   "T" 'counsel-load-theme

   "a" '(:ignore t :which-key "Applications")
   "ac" 'calendar
   "ad" 'counsel-dired
   "am" 'emms
   "ak" 'calc
   "ah" 'rajasegar/open-hackernews
   "ap" 'prodigy
   "ar" 'elfeed
   "at" 'eshell-extensions/open-new-eshell
   "aw" 'rajasegar/update-wallpaper


   "b" '(:ignore t :which-key "Buffers")
   "bb"  'ivy-switch-buffer
   "bd" 'kill-this-buffer
   "be" 'eval-buffer
   "bh" 'rajasegar/switch-to-dashboard
   "bn" 'centaur-tabs-forward
   "bp" 'centaur-tabs-backward
   "br" 'rename-buffer
   "bs" 'scratch-buffer
   "bx" 'erase-buffer

   "c" '(:ignore t :which-key "Comment")
   "cl" 'comment-line
   "cg" 'counsel-rg

   "d" '(:ignore t :which-key "Comments")
   "df" 'js-doc-insert-function-doc
   "dm" 'js-doc-insert-file-doc

   "e" '(:ignore t :which-key "Edit Configs")
   "e." 'rajasegar/open-emacs-config-folder
   "ef" 'rajasegar/edit-emacs-functions
   "ek" 'rajasegar/edit-emacs-keybindings
   "ep" 'rajasegar/edit-emacs-packages
   "er" 'rajasegar/run-rcup
   "es" 'rajasegar/edit-emacs-settings

   "f" '(:ignore t :which-key "Files")
   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fs" 'save-buffer
   
   ;; Flymake
   "fn" 'flymake-goto-next-error
   ;; "fp" 'flymake-goto-prev-error
   "fd" 'flymake-show-buffer-diagnostics
   

   "g" '(:ignore t :which-key "Git")
   "gb" 'magit-blame
   "gc" 'rajasegar/compare-git-branches
   "gf" 'rajasegar/stage-file-in-current-line
   "gg" 'rajasegar/create-gist
   "gh" 'rajasegar/switch-git-personal
   "gi" 'rajasegar/open-github-issues
   "gm" 'rajasegar/open-github-pull-requests
   "gn" 'rajasegar/open-new-pull-request
   "go" 'rajasegar/open-project-in-github
   "gp" 'magit-push-current-to-upstream
   "gs" 'magit-status
   "gt" 'git-timemachine
   "gw" 'rajasegar/switch-git-work
   "gz" 'rajasegar/magit-stash-untracked

   "h" '(:ignore t :which-key "Help")
   "hb" 'counsel-descbinds
   "hf" 'counsel-describe-function
   "hv" 'counsel-describe-variable

   "i" 'ielm

   "j" '(:ignore t :which-key "Jump-Buffer")
   "jj" 'switch-to-buffer
   "jh" 'switch-to-prev-buffer
   "jl" 'switch-to-next-buffer
   "jc" 'rajasegar/jump-to-component
   "jt" 'rajasegar/jump-to-template

   "k" 'eval-expression

   "l" '(:ignore t :whick-key "LSP")
   "li" 'eglot-find-implementation
   "lt" 'eglot-find-typeDefinition
   "ld" 'eglot-find-declaration

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
   

   "o" '(:ignore t :which-key "Org-Mode")
   "oa" 'org-agenda
   "oc" 'org-capture
   "op" 'org-present
   "ot" 'org-toggle-link-display
   "os" 'org-timer-set-timer
   "ox" 'org-timer-stop

   "p" 'projectile-command-map
   "pf" 'counsel-projectile-find-file
   "pp" 'rajasegar/find-projects
   ;; "pp" 'projectile-switch-project
   "pt" 'neotree-show

   "qq" 'kill-emacs
   "qr" 'restart-emacs

   "s" '(:ignore t :which-key "Slime")
   "ss" 'slime
   "sl" 'slime-load-file
   "sw" 'rajasegar/counsel-rg-word

   "t" '(:ignore t :which-key "Tabs & Toggles")
   "tc" 'rajasegar/add-codeium-completions
   "te" 'treesit-explore-mode
   "tl" 'toggle-truncate-lines
   "tn" 'tab-bar-new-tab
   "tp" 'tab-bar-switch-to-prev-tab
   "tq" 'tree-sitter-query-builder

   "v" '(:ignore t :which-key "Eshell")
   "vc" 'eshell-extensions/eshell-command-current-line
   "vr" 'eshell-command
   "vl" 'eshell-extensions/run-previous-eshell-command
   "vk" 'eshell-extensions/kill-buffer-eshell-command-output
   "vv" 'eshell-extensions/eshell-vertical
   "vh" 'eshell-extensions/eshell-horizontal

   "w" '(:ignore t :which-key "Window")
   "w-"  'split-window-below
   "w/"  'split-window-right
   "wd"  'delete-window
   "wh"  'windmove-left
   "wj"  'windmove-down
   "wk"  'windmove-up
   "wl"  'windmove-right
   "ww"  'evil-window-next

   "x" '(:ignore t :which-key "Execute")
   "xe" 'rajasegar/eval-print-last-sexp-no-truncation
   "xl" 'eval-last-sexp
   "xp" 'rajasegar/create-prodigy-service

   "y" '(:ignore t :which-key "Yasnippet")
   "yn" 'yas-new-snippet
   "yi" 'yas-insert-snippet

   "`" '(:ignore t :which-key "Freddy AI")
   "`p" 'freddy-ai/prompt
   "`r" 'freddy-ai/rephrase-from-region
   "`l" 'freddy-ai/prompt-from-line
   "`c" 'freddy-ai/write-code
   "`t" 'freddy-ai/write-tests
   )

  (general-define-key
   :states '(normal visual)
   :keymaps 'dired-mode-map
   "p" 'emms-play-dired
   "S" 'rajasegar/dired-sort-size
   "s" 'rajasegar/dired-sort-size-reverse
   )

  (general-define-key
   :states '(visual)
   "gc" 'evilnc-comment-or-uncomment-lines)

  (general-define-key
   :states '(normal visual)
   :keymaps 'neotree-mode-map
   "o" 'neotree-quick-look))

(provide 'keybindings)

;;; keybindings.el ends here
