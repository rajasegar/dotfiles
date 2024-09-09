;;; keybindings.el --- Rajasegar Chandran

;;; Commentary:
;; All my custom keybindings are defined here

;;; Code:
     
(global-set-key [f5] 'emms-start)
(global-set-key [f6] 'emms-stop)
(global-set-key [f7] 'emms-previous)
(global-set-key [f8] 'emms-next)

;; Hyper key bindings

(global-set-key (kbd "C-M-s-f") 'project-find-file)
(global-set-key (kbd "C-M-s-i") 'raja/open-iterm)
(global-set-key (kbd "C-M-s-g") 'rajasegar/open-google-chrome)
(global-set-key (kbd "C-M-s-m") 'next-buffer)
(global-set-key (kbd "C-M-s-n") 'previous-buffer)
(global-set-key (kbd "C-M-s-s") 'rajasegar/open-slack)
(global-set-key (kbd "C-M-s-t") 'rajasegar/open-alacritty)
(global-set-key (kbd "C-M-s-x") 'delete-other-windows)


;; Projects
(global-set-key (kbd "C-x p t") 'treemacs)
(global-set-key (kbd "C-x p a") 'treemacs-add-project-to-workspace)

;; Yasnippets
;; (global-set-key (kbd "C-c y n") 'yas-new-snippet)
;; (global-set-key (kbd "C-c y i") 'yas-insert-snippet)

;; Freddy
(global-set-key (kbd "C-c ` p") 'freddy-ai/prompt)
(global-set-key (kbd "C-c ` r") 'freddy-ai/rephrase-from-region)
(global-set-key (kbd "C-c ` l") 'freddy-ai/prompt-from-line)
(global-set-key (kbd "C-c ` c") 'freddy-ai/write-code)
(global-set-key (kbd "C-c ` e") 'freddy-ai/explain-code)
(global-set-key (kbd "C-c ` t") 'freddy-ai/write-tests)


;; Define key maps that will then be added to the prefix map
(defvar-keymap my-prefix-applications-map
  :doc "My prefix key map for applications."
  "c" #'calendar
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
  "h" #'rajasegar/switch-to-dashboard
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

(defvar-keymap my-prefix-git-map
  :doc "Git and Magit"
  "a" #'github-approve-pr
  "b" #'magit-blame
  "c" #'github-approve-current-pr
  "f" #'rajasegar/stage-file-in-current-line
  "g" #'rajasegar/create-gist
  "h" #'rajasegar/switch-git-personal
  "i" #'rajasegar/open-github-issues
  "l" #'github-list-pr-files
  "m" #'github-merge-pr
  "n" #'rajasegar/open-new-pull-request
  "o" #'rajasegar/open-project-in-github
  "p" #'magit-push-current-to-upstream
  "r" #'rajasegar/git-reverse-merge-dev-branch
  "s" #'magit-status
  "t" #'git-timemachine
  "w" #'rajasegar/switch-git-work
  "x" #'github-prs-repo
  "z" #'rajasegar/magit-stash-untracked)

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
  "b" #'emms-browser
  "c" #'rajasegar/play-college-folder
  "d" #'emms-play-directory-tree
  "f" #'rajasegar/play-favs-folder
  "l" #'rajasegar/play-latest-folder
  "n" #'emms-next
  "o" #'emms-show
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

(defvar-keymap my-prefix-react-map
  :doc "React migration C-c r"
"a" #'rm/replace-and-helpers      
"c" #'rm/rename-class-to-className
"e" #'rm/replace-eq-helpers
"n" #'rm/replace-not-helpers
"o" #'rm/replace-or-helpers
"i" #'rm/replace-if-block
"I" #'rm/replace-if-end-block     
"t" #'rm/replace-t-helper         
"u" #'rm/replace-unless-block     
"U" #'rm/replace-unless-inline    
"E" #'rm/replace-each-block)

(defvar-keymap my-prefix-slime-map
  :doc "Slime C-c s"
  "l" #'slime-load-file
  "s" #'slime
  "w" #'rajasegar/rg-word)

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
  "a" my-prefix-applications-map
  "b" my-prefix-buffers-map
  "c" my-prefix-c-map
  "d" #'duplicate-line
  "e" #'rajasegar/kill-to-eof
  "f" my-prefix-files-map
  "g" my-prefix-git-map
  "h" #'hs-toggle-hiding
  "i" #'ielm
  "j" my-prefix-jump-map
  "l" my-prefix-eglot-map
  "m" my-prefix-music-map
  "o" my-prefix-org-map
  "p" #'raja/plantuml-preview
  "q" my-prefix-q-map
  "r" my-prefix-react-map
  "s" my-prefix-slime-map
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


(defvar-keymap hs-repeat-map
  :repeat t
  "[" #'hs-show-block
  "]" #'hs-hide-block
  "h" #'hs-toggle-hiding)

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

(defvar-keymap duplicate-key-map
  :repeat t
  "d" #'rajasegar/duplicate-line)

(provide 'keybindings)

;;; keybindings.el ends here
