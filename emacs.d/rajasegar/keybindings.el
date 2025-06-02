;; -*- lexical-binding: t; -*-
;;; keybindings.el --- Rajasegar Chandran

;;; Commentary:
;; All my custom keybindings are defined here

;;; Code:
     
(global-set-key [f5] 'emms-start)
(global-set-key [f6] 'emms-stop)
(global-set-key [f7] 'emms-previous)
(global-set-key [f8] 'emms-next)


(defvar-keymap hs-repeat-map
  :repeat t
  "[" #'hs-show-block
  "]" #'hs-hide-block
  "h" #'hs-toggle-hiding)

(defvar-keymap tab-line-repeat-map
  :repeat t
  "j" #'tab-line-switch-to-next-tab
  "k" #'tab-line-switch-to-prev-tab)

;;; Leader
(define-prefix-command 'my-leader-map)

(keymap-set evil-visual-state-map "SPC" 'my-leader-map)
(keymap-set evil-normal-state-map "SPC" 'my-leader-map)

(evil-define-key nil my-leader-map
    ";"  'save-buffer
    ;; "x" 'eval-last-sexp
    "/" #'rg
    "-" 'split-window-below
    "\\" 'split-window-right
    "," #'diff-buffer-with-file
    "]" #'hs-hide-block
    "[" #'hs-show-block
    "C-r" #'ivy-resume
    "u" #'undo
    "j" #'tab-line-switch-to-next-tab
    "k" #'tab-line-switch-to-prev-tab
    
    ;; "My prefix key map for config"
    ".." #'(lambda () (interactive) (find-file user-init-file))
    ".d" #'my/open-emacs-config-folder
    ".k" #'my/edit-emacs-keybindings
    ".p" #'my/edit-emacs-packages
    ".f" #'my/edit-emacs-functions
    ".s" #'my/edit-emacs-settings

    ;; Gptel
    "``" #'gptel
    "`r" #'gptel-rewrite
    "`s" #'gptel-send
    
  ;; My prefix key map for applications
  "ac" #'calendar
  "ad" #'dired
  "ae" #'newsticker-treeview
  "af" #'rajasegar/open-firefox
  "am" #'emms
  "ak" #'raja/kanata
  "ah" #'rajasegar/open-hackernews
  "ap" #'prodigy
  "as" #'raja/startup
  "at" #'eshell
  "aw" #'rajasegar/update-wallpaper

  ;; "My prefix key map for buffers."
  "bb" #'switch-to-buffer
  "bc" #'rajasegar/copy-buffer
  "bd" #'evil-delete-buffer
  "be" #'eval-buffer
  "bh" #'rajasegar/switch-to-dashboard
  "bi" #'raja/indent-whole-buffer
  "br" #'rename-buffer
  "bs" #'scratch-buffer

  ;; "c prefix"
  "cg" #'rg
  "cc" #'raja/compile-c
  "cl" 'comment-line
  "cr" #'raja/run-c

  ;; Eglot
  "ed" #'eglot-find-declaration
  "ei" #'eglot-find-implementation
  "et" #'eglot-find-typeDefinition

  ;; "Files and flymake"
  "fd" #'flymake-show-buffer-diagnostics
  "ff" 'find-file
  "fj" #'flymake-goto-next-error
  "fk" #'flymake-goto-prev-error
  "fr" #'recentf-open-files

  ;; "Git and Magit"
  "ga" #'github-approve-pr
  "gb" #'magit-blame
  "gc" #'github-approve-current-pr
  "gf" #'rajasegar/stage-file-in-current-line
  "gg" #'rajasegar/create-gist
  "gh" #'rajasegar/switch-git-personal
  "gi" #'rajasegar/open-github-issues
  "gl" #'github-list-pr-files
  "gm" #'github-merge-pr
  "gn" #'rajasegar/open-new-pull-request
  "go" #'rajasegar/open-project-in-github
  "gp" #'magit-push-current-to-upstream
  "gr" #'rajasegar/git-reverse-merge-dev-branch
  "gs" #'magit-status
  "gt" #'git-timemachine
  "gw" #'rajasegar/switch-git-work
  "gx" #'github-prs-repo
  "gz" #'rajasegar/magit-stash-untracked

  ;; Music and Emms
  "mb" #'emms-browser
  "mc" #'rajasegar/play-college-folder
  "md" #'emms-play-directory-tree
  "mf" #'rajasegar/play-favs-folder
  "ml" #'rajasegar/play-latest-folder
  "mn" #'emms-next
  "mo" #'emms-show
  "mp" #'emms-previous
  "ms" #'emms-shuffle
  "mx" #'emms-pause

  ;; Org mode
  "oa" #'org-agenda
  "oc" #'org-capture
  "op" #'org-present
  "os" #'org-timer-set-timer
  "ot" #'org-toggle-link-display
  "ox" #'org-timer-stop

    ;; Bookmarks
    "rm" #'bookmark-set
    "rb" #'bookmark-jump

  ;; Slime
  "sl" #'slime-load-file
  "ss" #'slime
  "sw" #'rajasegar/rg-word

  ;; Toggles and tree sitter
  "te" #'treesit-explore-mode
  "ti" #'treesit-inspect-mode
  "tl" #'toggle-truncate-lines
  "tq" #'tree-sitter-query-builder
  "tr" #'raja/toggle-relative-line-number
  "t-" #'text-scale-decrease
  "t=" #'text-scale-increase

  ;; Window
  "wd" #'delete-window
  "wh" #'windmove-left
  "wj" #'windmove-down
  "wk" #'windmove-up
  "wl" #'windmove-right
  "ws" #'window-swap-states
  "w1" #'delete-other-windows
  "w2" #'split-window-below
  "w3" #'split-window-right
  "w0" #'delete-window
  "ww" #'other-window

  ;; Execute
  ;; "xe" #'rajasegar/eval-print-last-sexp-no-truncation
  ;; "xp" #'rajasegar/create-prodigy-service

  ;; Quit emacs
  "qq" #'kill-emacs
  "qr" #'restart-emacs
  )

(define-key evil-normal-state-map (kbd "SPC h") help-map)
(define-key evil-normal-state-map (kbd "SPC x") ctl-x-map)
(define-key evil-normal-state-map (kbd "SPC p") project-prefix-map)
(define-key evil-normal-state-map (kbd "SPC p t") 'treemacs)
(define-key evil-normal-state-map (kbd "SPC p a") 'treemacs-add-project-to-workspace)
(define-key evil-normal-state-map (kbd "SPC v") vc-prefix-map)


(provide 'keybindings)

;;; keybindings.el ends here
