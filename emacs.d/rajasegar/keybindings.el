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

;; (global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c /") 'counsel-rg)
(global-set-key (kbd "C-c ,") 'diff-buffer-with-file)
(global-set-key (kbd "C-c '") 'raja/search-word-in-project)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; Hyper key bindings
(global-set-key (kbd "C-M-s-x") 'delete-other-windows)
(global-set-key (kbd "C-M-s-t") 'rajasegar/open-alacritty)
(global-set-key (kbd "C-M-s-i") 'raja/open-iterm)
(global-set-key (kbd "C-M-s-f") 'rajasegar/open-firefox)
(global-set-key (kbd "C-M-s-s") 'rajasegar/open-slack)
(global-set-key (kbd "C-M-s-g") 'rajasegar/open-google-chrome)
;; (global-set-key (kbd "C-M-s-q") 'tab-close)



;; Applications
(global-set-key (kbd "C-c a c") 'calendar)
(global-set-key (kbd "C-c a d") 'counsel-dired)
(global-set-key (kbd "C-c a m") 'emms)
(global-set-key (kbd "C-c a k") 'calc)
(global-set-key (kbd "C-c a h") 'rajasegar/open-hackernews)
(global-set-key (kbd "C-c a p") 'prodigy)
(global-set-key (kbd "C-c a r") 'elfeed)
(global-set-key (kbd "C-c a t") 'eshell-extensions/open-new-eshell)
(global-set-key (kbd "C-c a w") 'rajasegar/update-wallpaper)

;; Buffers
(global-set-key (kbd "C-c b c") 'rajasegar/copy-buffer)
(global-set-key (kbd "C-c b d") 'kill-this-buffer)
(global-set-key (kbd "C-c b e") 'eval-buffer)
(global-set-key (kbd "C-c b h") 'rajasegar/switch-to-dashboard)
(global-set-key (kbd "C-c b r") 'rename-buffer)
(global-set-key (kbd "C-c b s") 'scratch-buffer)
(global-set-key (kbd "C-c b e") 'erase-buffer)


(global-set-key (kbd "C-c c g") 'counsel-rg)

(global-set-key (kbd "C-c d") 'duplicate-line)

;; Files and Flymake
(global-set-key (kbd "C-c f r") 'counsel-recentf)
(global-set-key (kbd "C-c f n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c f d") 'flymake-show-buffer-diagnostics)
   
;; Git
(global-set-key (kbd "C-c g a") 'github-approve-pr)
(global-set-key (kbd "C-c g b") 'magit-blame)
(global-set-key (kbd "C-c g c") 'github-approve-current-pr)
(global-set-key (kbd "C-c g f") 'rajasegar/stage-file-in-current-line)
(global-set-key (kbd "C-c g g") 'rajasegar/create-gist)
(global-set-key (kbd "C-c g h") 'rajasegar/switch-git-personal)
(global-set-key (kbd "C-c g i") 'rajasegar/open-github-issues)
(global-set-key (kbd "C-c g l") 'github-list-pr-files)
(global-set-key (kbd "C-c g m") 'github-merge-pr)
(global-set-key (kbd "C-c g n") 'rajasegar/open-new-pull-request)
(global-set-key (kbd "C-c g o") 'rajasegar/open-project-in-github)
(global-set-key (kbd "C-c g p") 'magit-push-current-to-upstream)
(global-set-key (kbd "C-c g r") 'rajasegar/git-reverse-merge-dev-branch)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g t") 'git-timemachine)
(global-set-key (kbd "C-c g w") 'rajasegar/switch-git-work)
(global-set-key (kbd "C-c g x") 'github-prs-repo)
(global-set-key (kbd "C-c g z") 'rajasegar/magit-stash-untracked)



(global-set-key (kbd "C-c i") 'ielm)


;; Jump 
(global-set-key (kbd "C-c j c") 'rajasegar/jump-to-component)
(global-set-key (kbd "C-c j t") 'rajasegar/jump-to-template)
(global-set-key (kbd "C-c j h") 'previous-buffer)
(global-set-key (kbd "C-c j l") 'next-buffer)
(global-set-key (kbd "C-M-s-n") 'previous-buffer)
(global-set-key (kbd "C-M-s-m") 'next-buffer)

(global-set-key (kbd "C-c k") 'eval-expression)

;; Eglot
(global-set-key (kbd "C-c l i") 'lsp-goto-implementation)
(global-set-key (kbd "C-c l d") 'lsp-find-definition)
(global-set-key (kbd "C-c l t") 'lsp-find-type-definition)

;; Emms
(global-set-key (kbd "C-c m b") 'emms-browser)
(global-set-key (kbd "C-c m d") 'emms-play-directory-tree)
(global-set-key (kbd "C-c m n") 'emms-next)
(global-set-key (kbd "C-c m o") 'emms-show)
(global-set-key (kbd "C-c m p") 'emms-previous)
(global-set-key (kbd "C-c m s") 'emms-shuffle)
(global-set-key (kbd "C-c m x") 'emms-pause)
(global-set-key (kbd "C-c m f") 'rajasegar/play-favs-folder)
(global-set-key (kbd "C-c m c") 'rajasegar/play-college-folder)
(global-set-key (kbd "C-c m l") 'rajasegar/play-latest-folder)

;; Org mode
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o p") 'org-present)
(global-set-key (kbd "C-c o t") 'org-toggle-link-display)
(global-set-key (kbd "C-c o s") 'org-timer-set-timer)
(global-set-key (kbd "C-c o x") 'org-timer-stop)

;; Plantuml preview
(global-set-key (kbd "C-c p") 'raja/plantuml-preview)

;; Projects
(global-set-key (kbd "C-x p t") 'treemacs)

;; React migration
(global-set-key (kbd "C-c r a") 'rm/replace-and-helpers)
(global-set-key (kbd "C-c r e") 'rm/replace-eq-helpers)
(global-set-key (kbd "C-c r n") 'rm/replace-not-helpers)
(global-set-key (kbd "C-c r o") 'rm/replace-or-helpers)
(global-set-key (kbd "C-c r i") 'rm/replace-if-block)
(global-set-key (kbd "C-c r u") 'rm/replace-unless-block)
(global-set-key (kbd "C-c r c") 'rm/replace-each-block)

;; Slime
(global-set-key (kbd "C-c s s") 'slime)
(global-set-key (kbd "C-c s l") 'slime-load-file)
(global-set-key (kbd "C-c s w") 'rajasegar/counsel-rg-word)
   

;; Toggles and tree sitter
(global-set-key (kbd "C-c t c") 'rajasegar/add-codeium-completions)
(global-set-key (kbd "C-c t e") 'treesit-explore-mode)
(global-set-key (kbd "C-c t i") 'treesit-inspect-mode)
(global-set-key (kbd "C-c t l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c t q") 'tree-sitter-query-builder)

;; Eshell 
(global-set-key (kbd "C-c v c") 'eshell-extensions/eshell-command-current-line)
(global-set-key (kbd "C-c v r") 'eshell-command)
(global-set-key (kbd "C-c v l") 'eshell-extensions/run-previous-eshell-command)
(global-set-key (kbd "C-c v k") 'eshell-extensions/kill-buffer-eshell-command-output)
(global-set-key (kbd "C-c v v") 'eshell-extensions/eshell-vertical)
(global-set-key (kbd "C-c v h") 'eshell-extensions/eshell-horizontal)

;; Window movement
(global-set-key (kbd "C-c w h") 'windmove-left)
(global-set-key (kbd "C-c w j") 'windmove-down)
(global-set-key (kbd "C-c w k") 'windmove-up)
(global-set-key (kbd "C-c w l") 'windmove-right)


(global-set-key (kbd "C-c x e") 'rajasegar/eval-print-last-sexp-no-truncation)
(global-set-key (kbd "C-c x p") 'rajasegar/create-prodigy-service)

(global-set-key (kbd "C-c y") 'rajasegar/copy-line)
;; Yasnippets
;; (global-set-key (kbd "C-c y n") 'yas-new-snippet)
;; (global-set-key (kbd "C-c y i") 'yas-insert-snippet)

;; Quit
(global-set-key (kbd "C-c q q") 'kill-emacs)
(global-set-key (kbd "C-c q r") 'restart-emacs)



;; Freddy
(global-set-key (kbd "C-c ` p") 'freddy-ai/prompt)
(global-set-key (kbd "C-c ` r") 'freddy-ai/rephrase-from-region)
(global-set-key (kbd "C-c ` l") 'freddy-ai/prompt-from-line)
(global-set-key (kbd "C-c ` c") 'freddy-ai/write-code)
(global-set-key (kbd "C-c ` e") 'freddy-ai/explain-code)
(global-set-key (kbd "C-c ` t") 'freddy-ai/write-tests)

(provide 'keybindings)

;;; keybindings.el ends here
