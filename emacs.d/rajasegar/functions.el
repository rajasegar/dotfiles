;;; functions.el --- My custom Emacs Lisp functions
; Edit this config

;;; Commentary:
;; This file contains both interactive and non-interactive functions
;; that are key bound to keys in the keybindings.el file

;;; Code:

(defun rajasegar/edit-emacs-configuration ()
  "Edit Emacs configuration."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun rajasegar/edit-emacs-settings ()
  "Open common settings file."
  (interactive)
  (find-file "~/.emacs.d/rajasegar/settings.el"))

(defun rajasegar/edit-emacs-packages ()
  "Open  packages file."
  (interactive)
  (find-file "~/.emacs.d/rajasegar/packages.el"))

(defun rajasegar/edit-emacs-keybindings ()
  "Open  keybindings file."
  (interactive)
  (find-file "~/.emacs.d/rajasegar/keybindings.el"))

(defun rajasegar/edit-emacs-functions ()
  "Open  keybindings file."
  (interactive)
  (find-file "~/.emacs.d/rajasegar/functions.el"))

(defun rajasegar/open-emacs-config-folder ()
  "Open Emacs config folder."
  (interactive)
  (dired "~/.emacs.d/rajasegar"))

(defun rajasegar/toggle-buffers ()
  "Toggle buffers."
  (interactive)
  (switch-to-buffer nil))

(defun rajasegar/switch-to-dashboard ()
 "Switch to dashboard buffer."
  (interactive)
  (switch-to-buffer dashboard-buffer-name))

(defun rajasegar/switch-git-personal ()
  "Switch to personal Github profile."
  (interactive)
  (shell-command "ssh-add -D && ssh-add ~/.ssh/id_ed25519 && ssh -T git@github.com"))

(defun rajasegar/switch-git-work ()
  "Switch to work Github profile."
  (interactive)
  (shell-command "ssh-add -D && ssh-add ~/.ssh/freshworks && ssh -T git@github.com"))

(defun rajasegar/play-favs-folder ()
  "Play the Favs directory in EMMS."
  (interactive)
  (emms-play-directory-tree "~/Music/Favs"))

(defun rajasegar/play-college-folder ()
  "Play the College directory in EMMS."
  (interactive)
  (emms-play-directory-tree "~/Music/College"))

(defun rajasegar/play-latest-folder ()
  "Play the 2023 directory in EMMS."
  (interactive)
  (emms-play-directory-tree "~/Music/2023"))

(defun rajasegar/open-hackernews ()
  "OPen hacker news website in eww."
  (interactive)
  (eww "hackernews.com"))


(defun rajasegar/add-codeium-completions ()
  "Add codeium completions to the current buffer."
  (interactive)
  ;; (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  (setq completion-at-point-functions (list 'codeium-completion-at-point)))

(defun get-projects ()
  "Get the list of recently visited projects."
 (with-temp-buffer
    (insert-file-contents "~/.emacs.d/projects")
    (let* ((projects (read (current-buffer)))
          (props '()))
      (loop for (p) in projects do
            (add-to-list 'props p))
      props)))

(defun rajasegar/find-projects-function (str pred _)
  "Callback function for `ivy-read' for projects list.
Argument STR string.
Argument PRED predicate."
  (let* ((props (get-projects))
        (strs (cl-mapcar (lambda (p) (car (last (split-string p "/" t)))) props)))
    (cl-mapcar (lambda (s p) (propertize s 'property p))
               strs
               props)))

  

(defun rajasegar/find-projects ()
  "Find the projects."
  (interactive)
  (tab-bar-new-tab)
  (ivy-read "Find projects: "
            #'rajasegar/find-projects-function
            :action (lambda (x)
                      (project-switch-project (get-text-property 0 'property x))
                      (tab-bar-rename-tab x))))

(defun rajasegar/open-project ()
  "Create a new tab and switch project."
  (interactive)
  (tab-bar-new-tab)
  (projectile-switch-project))

(defun github-repository-url ()
  "Get the github repository url of the current file."
  (string-replace
   ".git"
   ""
   (string-replace
    "git@"
    "https://"
    (string-replace  ":" "/" (vc-git-repository-url (buffer-file-name))))))


(defun rajasegar/open-new-pull-request ()
  "Open new pull request url for current branch in browser."
  (interactive)
  (shell-command (concat "open " (github-repository-url) "/pull/new/" (car (vc-git-branches)))))

(defun rajasegar/compare-git-branches ()
  "Open compare branches page in github in the browser."
  (interactive)
  (shell-command (concat "open " (github-repository-url) "/compare/dev..." (car (vc-git-branches)))))

(defun prodigy-port (task)
  "Return port number for the respective task, otherwise 8080"
  (cond
    ((string= task "vite") 5173)
    ((string= task "vite preview") 4173)
    (t 8080)
    )
  )


(defun rajasegar/create-prodigy-service (&optional package-manager)
  "Create new prodigy services based on current package.json.
Optional argument PACKAGE-MANAGER The type of package manager to use (default: pnpm)."
  (interactive)
  (let ((pkg (json-parse-string (buffer-substring-no-properties (point-min) (point-max)))))
    (maphash  (lambda (key value)
                (let ((args '())
                      (name (gethash "name" pkg)))
                  (add-to-list 'args key)
                  (add-to-list 'args "run")
                  (prodigy-define-service
                    :name (concat name "-" key)
                    :command (or package-manager "pnpm")
                    :cwd (file-name-directory (buffer-file-name))
                    :path (file-name-directory (buffer-file-name))
                    :args args
                    :tags '(temp)
                    :stop-signal 'sigkill
                    :kill-process-buffer-on-stop t
                    :port (prodigy-port value)
                    ))) (gethash "scripts" pkg))
    ;; Open prodigy and refresh
    (prodigy)
    (prodigy-refresh)))

(defun rajasegar/stage-file-in-current-line ()
  "Magit Stage the file name in the current line."
  (interactive)
  (let ((filename (string-trim (buffer-substring-no-properties (+ 2 (line-beginning-position)) (line-end-position)))))
    (message "Staging file: %s" filename)
    (magit-stage-file filename)
    (message "File staged successfully: %s !!" filename)))

(defun rajasegar/magit-stash-untracked ()
  "Stash include untracked files using magit."
  (interactive)
  (magit-stash-both (read-string "Enter stash name: ") t))

(defun my-project-root ()
  "Find the project root."
    (locate-dominating-file (file-name-directory (buffer-file-name)) "package.json"))

(defun rajasegar/jump-to-component ()
  "Jump to the corresponding Ember component file from test file."
  (interactive)
  (let ((root-dir (my-project-root))
        (component (string-replace "-test" "" (file-name-base (buffer-file-name)))))
  (find-file (concat root-dir "/app/components/" component "/component.js"))))

(defun rajasegar/jump-to-template ()
  "Jump to the corresponding Ember component hbs file from test file."
  (interactive)
  (let ((root-dir (my-project-root))
        (component (string-replace "-test" "" (file-name-base (buffer-file-name)))))
  (find-file (concat root-dir "/app/components/" component "/template.hbs"))))

(defun rajasegar/counsel-rg-word ()
  "Search word under cursor using `counsel-rg'."
  (interactive)
  (counsel-rg (word-at-point)))

(defun rajasegar/run-rcup ()
  "Run rcup -v to update dotfiles."
  (interactive)
  (async-shell-command "rcup -v")
  (switch-to-buffer-other-window "*Async Shell Command*"))


(defun rajasegar/create-gist ()
  "Create gist from current buffer."
  (interactive)
  (eshell-command (concat "gh gist create "  (file-name-base (buffer-file-name)) "." (file-name-extension (buffer-file-name)) )))

(defun rajasegar/update-wallpaper ()
  "Fetch a new wallpaper from Upsplash and update it."
  (interactive)
  (eshell-command (concat "wallpaper.sh")))


(defun rajasegar/apt-get-install ()
  "Read a package name from minibuffer and install it with apt-get."
  (interactive)
  (eshell-command (concat "sudo apt-get -y install " (read-string "Enter the package name: "))))

(defun rajasegar/open-project-in-github ()
  "Open the github url of the current project"
  (interactive)
  (shell-command (concat "open " (github-repository-url))))

(defun rajasegar/open-github-pull-requests ()
  "Open the github pull requests url of the current project"
  (interactive)
  (shell-command (concat "open " (github-repository-url) "/pulls")))

(defun rajasegar/open-github-issues ()
  "Open the github issues url of the current project"
  (interactive)
  (shell-command (concat "open " (github-repository-url) "/issues")))

(defun rajasegar/dired-sort-size ()
  "Dired sort by size."
  (interactive)
  (dired-sort-other  "-lS"))

(defun rajasegar/dired-sort-size-reverse ()
  "Dired sort by size."
  (interactive)
  (dired-sort-other  "-lrS"))

(defun rajasegar/eval-print-last-sexp-no-truncation ()
  "Eval sexp and print without truncating the output"
  (interactive)
  (eval-print-last-sexp 0))

(defun rajasegar/copy-buffer ()
  (interactive)
  ;; (mark-whole-buffer)
  (copy-region-as-kill (point-min) (point-max)))

(defun rajasegar/copy-line ()
  (interactive)
  (copy-region-as-kill (point-at-bol) (point-at-eol)))

(defun rajasegar/delete-current-line()
  "Delete the current line under the cursor"
  (interactive)
  (kill-region (point-at-bol) (point-at-eol)))

(defun rajasegar/duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun rajasegar/kill-to-eof ()
  "Kill from the current cursor position to end of file"
  (interactive)
  (kill-region (point) (point-max)))

(defun rajasegar/export-svg-as-png ()
  "Export svg files as png using Inkscape"
  (let ((svg-file ())))
  (async-shell-command (concat )"/Applications/Inkscape.app/Contents/MacOS/inkscape --export-type png --export-filename output.png marketplace-adapter.svg "))

(defun rajasegar/open-alacritty ()
  "Open Alacritty terminal from Emacs"
  (interactive)
  (shell-command "open -a Alacritty.app"))

(defun rajasegar/open-firefox ()
  "Open Firefox browser from Emacs"
  (interactive)
  (shell-command "open -a Firefox.app"))

(defun rajasegar/open-slack ()
  "Open Slack app from Emacs"
  (interactive)
  (shell-command "open -a Slack.app"))

(defun rajasegar/open-google-chrome ()
  "Open Google Chrome browser from Emacs"
  (interactive)
  (shell-command "open -a 'Google Chrome.app'"))

(defun raja/startup ()
  "Start apps in Mac in a bunch from Emacs"
  (interactive)
  (shell-command "open -a Firefox.app")
  (shell-command "open -a Slack.app")
    (shell-command "open -a 'Google Chrome.app'"))

(defun rajasegar/ember-plantuml ()
  "Open Ember plantuml for Freshchat"
  (interactive)
  (async-shell-command "cd ~/www/ember-plantuml;./bin/ember-plantuml.js"))

(defun raja/search-word-in-project ()
  "Search the word under the cursor in  project"
  (interactive)
  (project-find-regexp (thing-at-point 'word t)))

(defun raja/plantuml-preview ()
  "Preview plantuml"
  (interactive)
  (shell-command (concat "java -jar ~/plantuml.jar " (buffer-file-name)))
  (find-file (string-replace ".pum" ".png" (buffer-file-name))))

(defun raja/compile-c ()
  "Compile c programs with cc"
  (interactive)
  (compile (format "cc -o %s %s" (file-name-base (buffer-file-name)) (buffer-file-name))))

(defun raja/run-c ()
  "Run the compiled C program"
  (interactive)
  (shell-command (format "./%s" (file-name-base (buffer-file-name)))))

(defun raja/eno-fd ()
  "Degit the ember-new-output repo for Freshdesk"
  (interactive)
  (async-shell-command (format "degit ember-cli/ember-new-output#v3.8.3 eno-%s" (make-temp-name "fd-"))))

(defun raja/eno-fc ()
  "Degit the ember-new-output repo for Freshchat"
  (interactive)
  (async-shell-command (format "degit ember-cli/ember-new-output#v3.20.2 eno-%s" (make-temp-name "fc-"))))

(defun raja/toggle-relative-line-number ()
  "Toggle relative line number mode"
  (interactive)
  (if (eq display-line-numbers-type 'relative)
      (setq display-line-numbers-type t)
    (setq display-line-numbers-type 'relative))
  (display-line-numbers--turn-on)
  )

(defun raja/indent-whole-buffer ()
  "Mark the current buffer entirely and indent it"
  (interactive)
  (indent-region (point-min) (point-max))
  )

(provide 'functions)

;;; functions.el ends here
