; Edit this config
(defun rajasegar/edit-emacs-configuration ()
  "Edit Emacs configuration."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun rajasegar/edit-emacs-settings ()
  "Open common settings file"
  (interactive)
  (find-file "~/.emacs.d/rajasegar/settings.el"))

(defun rajasegar/edit-emacs-packages ()
  "Open  packages file"
  (interactive)
  (find-file "~/.emacs.d/rajasegar/packages.el"))

(defun rajasegar/edit-emacs-keybindings ()
  "Open  keybindings file"
  (interactive)
  (find-file "~/.emacs.d/rajasegar/keybindings.el"))

(defun rajasegar/edit-emacs-functions ()
  "Open  keybindings file"
  (interactive)
  (find-file "~/.emacs.d/rajasegar/functions.el"))

(defun rajasegar/open-emacs-config-folder ()
  "Open Emacs config folder"
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
  "Play the Favs directory in EMMS"
  (interactive)
  (emms-play-directory-tree "~/Music/Favs"))

(defun rajasegar/play-college-folder ()
  "Play the College directory in EMMS"
  (interactive)
  (emms-play-directory-tree "~/Music/College"))

(defun rajasegar/play-latest-folder ()
  "Play the 2023 directory in EMMS"
  (interactive)
  (emms-play-directory-tree "~/Music/2023"))

(defun rajasegar/open-hackernews ()
  "OPen hacker news website in eww"
  (interactive)
  (eww "hackernews.com"))


(defun rajasegar/add-codeium-completions ()
  "add codeium completions to the current buffer"
  (interactive)
  ;; (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  (setq completion-at-point-functions (list 'codeium-completion-at-point)))

(defun get-projects ()
 (with-temp-buffer
    (insert-file-contents "~/.emacs.d/projects")
    (let* ((projects (read (current-buffer)))
          (props '()))
      (loop for (p) in projects do
            (add-to-list 'props p))
      props)))

(defun rajasegar/find-projects-function (str pred _)
  (let* ((props (get-projects))
        (strs (cl-mapcar (lambda (p) (car (last (split-string p "/" t)))) props)))
    (cl-mapcar (lambda (s p) (propertize s 'property p))
               strs
               props)))

  

(defun rajasegar/find-projects ()
  (interactive)
  (tab-bar-new-tab)
  (ivy-read "Find projects: "
            #'rajasegar/find-projects-function
            :action (lambda (x)
                      (project-switch-project (get-text-property 0 'property x))
                      (tab-bar-rename-tab x))))

(defun rajasegar/open-project ()
  "create a new tab and switch project"
  (interactive)
  (tab-bar-new-tab)
  (projectile-switch-project))


(defun rajasegar/open-new-pull-request ()
  "Open new pull request url for current branch in browser"
  (interactive)
  (shell-command (concat "open https://github.com/freshdesk/unity_frontend/pull/new/" (car (vc-git-branches)))))

(defun rajasegar/compare-git-branches ()
  "Open compare branches page in github in the browser"
  (interactive)
  (shell-command (concat "open https://github.com/freshdesk/unity_frontend/compare/dev..." (car (vc-git-branches)))))

(defun rajasegar/create-prodigy-service ()
  "Create new prodigy services based on current package.json"
  (interactive)
  (let ((pkg (json-parse-string (buffer-substring-no-properties (point-min) (point-max)))))
    (maphash  (lambda (key value)
                (let ((args '())
                      (name (gethash "name" pkg)))
                  (add-to-list 'args key)
                  (add-to-list 'args "run")
                  (prodigy-define-service
                    :name (concat name "-" key)
                    :command "npm"
                    :cwd (file-name-directory (buffer-file-name))
                    :path (file-name-directory (buffer-file-name))
                    :args args
                    :tags '(temp)
                    :stop-signal 'sigkill
                    :kill-process-buffer-on-stop t
                    ))) (gethash "scripts" pkg))
    (prodigy)))

(defun rajasegar/stage-file-in-current-line ()
  "Magit Stage the file name in the current line"
  (interactive)
  (let ((filename (string-trim (buffer-substring-no-properties (+ 2 (line-beginning-position)) (line-end-position)))))
    (message "Staging file: %s" filename)
    (magit-stage-file filename)
    (message "File staged successfully: %s !!" filename)))

(defun rajasegar/magit-stash-untracked ()
  "Stash include untracked files using magit"
  (interactive)
  (magit-stash-both (read-string "Enter stash name: ") t))

(defun my-project-root ()
    (locate-dominating-file (file-name-directory (buffer-file-name)) "package.json"))

(defun rajasegar/jump-to-component ()
  "Jump to the corresponding Ember component file from test file"
  (interactive)
  (let ((root-dir (my-project-root))
        (component (string-replace "-test" "" (file-name-base (buffer-file-name)))))
  (find-file (concat root-dir "/app/components/" component "/component.js"))))

(defun rajasegar/jump-to-template ()
  "Jump to the corresponding Ember component hbs file from test file"
  (interactive)
  (let ((root-dir (my-project-root))
        (component (string-replace "-test" "" (file-name-base (buffer-file-name)))))
  (find-file (concat root-dir "/app/components/" component "/template.hbs"))))

(defun rajasegar/run-rcup ()
  "Run rcup -v to update dotfiles."
  (interactive)
  (eshell-command  "rcup -v"))

