;; -*- lexical-binding: t; -*-
;;; keybindings.el --- Rajasegar Chandran

;;; Commentary:
;; All my custom keybindings are defined here

;;; Code:
     
(global-set-key [f9] 'emms-start)
(global-set-key [f7] 'emms-stop)
(global-set-key [f8] 'emms-previous)
(global-set-key [f10] 'emms-next)


(defvar-keymap hs-repeat-map
  :repeat t
  "[" #'hs-show-block
  "]" #'hs-hide-block
  "h" #'hs-toggle-hiding)

(defvar tab-line-switch-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'tab-line-switch-to-next-tab)
    (define-key map (kbd "k") #'tab-line-switch-to-prev-tab)
    map))

(dolist (cmd '(tab-line-switch-to-next-tab tab-line-switch-to-prev-tab))
  (put cmd 'repeat-map 'tab-line-switch-repeat-map))

;;; Leader
(define-prefix-command 'my-leader-map)

(keymap-set evil-visual-state-map "SPC" 'my-leader-map)
(keymap-set evil-normal-state-map "SPC" 'my-leader-map)

(evil-define-key nil my-leader-map
  ";"  'save-buffer
  ;; "x" 'eval-last-sexp
  "/" #'project-find-regexp
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
  ".e" #'my/open-eglot-config
  ".k" #'my/edit-emacs-keybindings
  ".p" #'my/edit-emacs-packages
  ".f" #'my/edit-emacs-functions
  ".s" #'my/edit-emacs-settings
  ".m" #'my/edit-emms-config

  ;; Gptel
  "`a" #'totefy/ffmpeg-add-audio
  "`c" #'ffmpeg/concat
  ;; "`c2" #'campaigns/2x2
  ;; "`c3" #'campaigns/3x3
  ;; "`cx" #'totefy/crop-images
  "`d" #'totefy/run-download-task
  "`i" #'totefy/create-insta-reel-sweatshirt
  "`m" #'totefy/generate-sweat-shirt-mockups
  "`p" #'totefy/run-printify-task
  "`r" #'totefy/insta-prepare-images
  "`s" #'totefy/whatsapp-story
  "`u" #'totefy/run-upload-task
  "`v" #'totefy/video-from-images
  "`w" #'totefy/run-woocommerce-task
  "`x" #'totefy/ffmpeg-xfade
  "`tm" #'totefy/ffmpeg-draw-multi-text
  "`tt" #'totefy/ffmpeg-draw-text
  
  ;; My prefix key map for applications
  "ac" #'calc
  "ad" #'dired-jump-other-window
  "ae" #'ellama-chat
  "ag" #'gptel
  "am" #'emms
  "ah" #'my/open-hackernews
  "ap" #'prodigy
  "an" #'newsticker-treeview
  "at" #'eshell-extensions-new
  "aw" #'my/update-wallpaper

  ;; "My prefix key map for buffers."
  "bb" #'switch-to-buffer
  "bc" #'my/copy-buffer
  "bd" #'(lambda () (interactive) (kill-current-buffer))
  "be" #'eval-buffer
  "bh" #'my/switch-to-dashboard
  "bi" #'raja/indent-whole-buffer
  "bj" #'my/run-with-node
  "bo" #'ido-switch-buffer-other-window
  "br" #'rename-buffer
  "bs" #'scratch-buffer
  "bt" #'my/run-with-tsc

  ;; "c prefix"
  "ca" #'ellama-code-add
  "ce" #'ellama-code-edit
  "cg" #'rg
  "cc" #'ellama-code-complete
  "cl" 'comment-line
  "cr" #'raja/run-c

  ;; Eglot
  "ed" #'eglot-find-declaration
  ;; "ei" #'eglot-find-implementation
  "el" #'eval-last-sexp
  "ep" #'my/eval-print-last-sexp-no-truncation
  "et" #'eglot-find-typeDefinition

  ;; Ellama
  "ee" #'ellama
  "ec" #'ellama-chat
  "ew" #'ellama-write
  "es" #'ellama-summarize
  "eig" #'ellama-improve-grammar
  "eiw" #'ellama-improve-wording
  "eic" #'ellama-improve-conciseness

  ;; "Files and flymake"
  "fc" #'ffmpeg/concat
  "fd" #'flymake-show-buffer-diagnostics
  "ff" 'find-file
  "fj" #'flymake-goto-next-error
  "fk" #'flymake-goto-prev-error
  "fr" #'recentf-open-files
  "fo" #'ido-find-file-other-window

  ;; Gptel
  "'g" #'gptel
  "'r" #'gptel-rewrite
  "'s" #'gptel-send
  "'t" #'gptel-tools
  "'m" #'gptel-menu
  "'x" #'gptel-abort

  ;; "Git and Magit"
  "ga" #'github-approve-pr
  "gb" #'magit-blame
  "gc" #'github-approve-current-pr
  "gf" #'rajasegar/stage-file-in-current-line
  "gg" #'rajasegar/create-gist
  "gh" #'rajasegar/switch-git-personal
  "gi" #'rajasegar/open-github-issues
  "gl" #'github-list-pr-files
  "gm" #'gimp-migrate
  "gn" #'rajasegar/open-new-pull-request
  "go" #'rajasegar/open-project-in-github
  "gp" #'gimp-create-palette
  "gr" #'rajasegar/git-reverse-merge-dev-branch
  "gs" #'magit-status
  "gt" #'git-timemachine
  "gw" #'rajasegar/switch-git-work
  "gx" #'github-prs-repo
  "gz" #'rajasegar/magit-stash-untracked

  ;; Music and Emms
  "mb" #'emms-browser
  "mc" #'my/play-college-folder
  "md" #'emms-play-directory-tree
  "mf" #'my/play-favs-folder
  "ml" #'my/play-latest-folder
  "mn" #'emms-next
  "mo" #'emms-metaplaylist-mode-go
  "mp" #'emms-previous
  "ms" #'emms-shuffle
  "mx" #'emms-pause
  "mm" #'emms

  ;; Mockup generator
  ;; Women
  ;; "mwb" #'mockup/women-bomber-jacket
  ;; "mwc" #'mockup/crop-top
  ;; "mwe" #'mockup/women-tee
  ;; "mwk" #'mockup/kaftan
  ;; "mwl" #'mockup/women-line-dress
  ;; "mwm" #'mockup/mini-skirt
  ;; "mws" #'mockup/sports-bra
  ;; "mwt" #'mockup/tank-top
  ;; "mwu" #'mockup/tube-top

  ;; Men
  ;; "mmr" #'mockup/rider-tshirt
  ;; "mt" #'mockup/tumbler
  ;; Kids
  ;; "mkt" #'mockup/kids-tshirt
  ;; "mkb" #'mockup/kids-bomber-jacket
  ;; "mks" #'mockup/kids-sweat-shirt
  
  ;; Org mode
  "oa" #'org-agenda
  "oc" #'org-capture
  "oi" #'org-toggle-inline-images
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
  "tt" #'my/make-temp-el

  ;; eshell extensions
  "vl" #'eshell-extensions-previous-command
  "vr" #'eshell-extensions-command

  ;; Window
  ;; "wd" #'delete-window
  ;; "wh" #'windmove-left
  ;; "wj" #'windmove-down
  ;; "wk" #'windmove-up
  ;; "wl" #'windmove-right
  ;; "ws" #'window-swap-states
  ;; "w1" #'delete-other-windows
  ;; "w2" #'split-window-below
  ;; "w3" #'split-window-right
  ;; "w0" #'delete-window
  ;; "ww" #'other-window

  ;; Wordpress
  "wr" #'my/wordpress-rider-tshirt

  ;; Execute
  ;; "xp" #'rajasegar/create-prodigy-service

  ;; Quit emacs
  "qq" #'kill-emacs
  "qr" #'restart-emacs

  ;; Printify key bindings
  "zs" #'printify/shops
  ;; "zb" #'printify/blueprints
  "zc" #'ffmpeg/zoomin-center
  "ztl" #'ffmpeg/zoomin-topleft
  "ztr" #'ffmpeg/zoomin-topright
  "zbb" #'ffmpeg-zoompan-batch
  "zbl" #'ffmpeg/zoomin-bottomleft
  "zbr" #'ffmpeg/zoomin-bottomright
  "zo" #'ffmpeg/zoomout-center
  
  )

;; (define-key evil-normal-state-map (kbd "SPC h") help-map)
;; (define-key evil-normal-state-map (kbd "SPC p") project-prefix-map)
(define-key evil-normal-state-map (kbd "SPC p t") 'treemacs)
(define-key evil-normal-state-map (kbd "SPC p a") 'treemacs-add-and-display-current-project-exclusively)
;; (define-key evil-normal-state-map (kbd "SPC v") vc-prefix-map)
;; (define-key evil-normal-state-map (kbd "SPC x") ctl-x-map)

(define-key evil-normal-state-map (kbd "[ t") 'tab-line-switch-to-prev-tab)
(define-key evil-normal-state-map (kbd "] t") 'tab-line-switch-to-next-tab)


;; "My prefix key map for editing dotfiles"
(defvar-keymap my-prefix-dot-map
  :doc "My prefix key map for editing dot files"
  "." #'(lambda () (interactive) (find-file user-init-file))
  "d" #'my/open-emacs-config-folder
  "e" #'my/open-eglot-config
  "k" #'my/edit-emacs-keybindings
  "p" #'my/edit-emacs-packages
  "f" #'my/edit-emacs-functions
  "s" #'my/edit-emacs-settings
  "m" #'my/edit-emms-config
  )


;; Define key maps that will then be added to the prefix map
(defvar-keymap my-prefix-applications-map
  :doc "My prefix key map for applications."
  "c" #'calc
  "d" #'dired
  "e" #'elfeed
  "f" #'rajasegar/open-firefox
  "m" #'emms
  "k" #'calc
  "h" #'rajasegar/open-hackernews
  "p" #'prodigy
  "s" #'raja/startup
  "t" #'eshell-extensions/open-new-eshell
  "w" #'rajasegar/update-wallpaper)

(defvar-keymap my-prefix-buffers-map
  :doc "Buffers"
  "c" #'rajasegar/copy-buffer
  "d" #'kill-this-buffer
  "e" #'eval-buffer
  "h" #'my/switch-to-dashboard
  "i" #'raja/indent-whole-buffer
  "r" #'rename-buffer
  "s" #'scratch-buffer)

(defvar-keymap my-prefix-c-map
  :doc "c prefix"
  "g" #'rg
  "c" #'raja/compile-c
  "r" #'raja/run-c)

(defvar-keymap my-prefix-files-map
  :doc "Files and flymake"
  :repeat t
  "d" #'flymake-show-buffer-diagnostics
  "j" #'flymake-goto-next-error
  "k" #'flymake-goto-prev-error
  "r" #'recentf-open-files)

(defvar-keymap my-prefix-jump-map
  :doc "Jump"
  "c" #'rajasegar/jump-to-component
  "h" #'previous-buffer
  "l" #'next-buffer
  "t" #'rajasegar/goto-to-template)


(defvar-keymap my-prefix-eglot-map
  :doc "Eglot"
  "d" #'eglot-find-declaration
  "i" #'eglot-find-implementation
  "t" #'eglot-find-typeDefinition)

(defvar-keymap my-prefix-music-map
  :doc "Music and Emms"
  :repeat t
  "a" #'emms-add-dired
  "b" #'emms-browser
  "c" #'my/play-college-folder
  "d" #'emms-play-directory-tree
  "e" #'emms-play-dired
  "f" #'my/play-favs-folder
  "l" #'my/play-latest-folder
  "n" #'emms-next
  "o" #'emms-playlist-mode-go
  "p" #'emms-previous
  "s" #'emms-shuffle
  "x" #'emms-pause)

(defvar-keymap my-prefix-org-map
  :doc "Org C-c o"
  "a" #'org-agenda
  "c" #'org-capture
  "p" #'org-present
  "s" #'org-timer-set-timer
  "t" #'org-toggle-link-display
  "x" #'org-timer-stop)

(defvar-keymap my-prefix-q-map
  :doc "Quit emacs C-c q"
  "q" #'kill-emacs
  "r" #'restart-emacs)



(defvar-keymap my-prefix-toggle-map
  :doc "Toggles and tree sitter C-c t"
  :repeat t
  "c" #'rajasegar/add-codeium-completions
  "e" #'treesit-explore-mode
  "i" #'treesit-inspect-mode
  "l" #'toggle-truncate-lines
  "q" #'tree-sitter-query-builder
  "r" #'raja/toggle-relative-line-number
  "-" #'text-scale-decrease
  "=" #'text-scale-increase)

(defvar-keymap my-prefix-v-map
  :doc "Eshell extensions C-c v"
  "c" #'vite/create
  "h" #'eshell-extensions/eshell-horizontal
  "k" #'eshell-extensions/kill-buffer-eshell-command-output
  "l" #'eshell-extensions/run-previous-eshell-command
  "r" #'eshell-command
  "v" #'eshell-extensions/eshell-vertical)

(defvar-keymap my-prefix-window-map
  :doc "Window C-c w"
  :repeat t
  "d" #'delete-window
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "s" #'window-swap-states)

(defvar-keymap my-prefix-x-map
  :doc "C-c x"
  "e" #'rajasegar/eval-print-last-sexp-no-truncation
  "p" #'rajasegar/create-prodigy-service)

(defvar-keymap my-prefix-map
  :doc "My C-c prefix key map."
  "." my-prefix-dot-map 
  "a" my-prefix-applications-map
  "b" my-prefix-buffers-map
  "c" my-prefix-c-map
  "d" #'duplicate-line
  "e" #'rajasegar/kill-to-eof
  "f" my-prefix-files-map
  ;; "g" my-prefix-git-map
  "h" #'hs-toggle-hiding
  "i" #'ielm
  "j" my-prefix-jump-map
  "l" my-prefix-eglot-map
  "m" my-prefix-music-map
  "o" my-prefix-org-map
  "p" #'raja/plantuml-preview
  "q" my-prefix-q-map
  ;; "r" my-prefix-react-map
  ;; "s" my-prefix-slime-map
  "t" my-prefix-toggle-map
  "u" #'rajasegar/delete-current-line
  "v" my-prefix-v-map
  "w" my-prefix-window-map
  "x" my-prefix-x-map
  "y" #'rajasegar/copy-line
  "z" #'zap-up-to-char
  "/" #'rg
  "," #'diff-buffer-with-file
  "'" #'raja/search-word-in-project
  "]" #'hs-hide-block
  "[" #'hs-show-block
  "C-r" #'ivy-resume)

(keymap-set global-map "C-c" my-prefix-map)



(defvar-keymap navigation-repeat-map
  :repeat t
  "n" #'next-line
  "p" #'previous-line
  "f" #'forward-word
  "b" #'backward-word
  "e" #'forward-sentence
  "a" #'backward-sentence
  "h" #'backward-char
  "l" #'forward-char)

(provide 'keybindings)

;;; keybindings.el ends here
