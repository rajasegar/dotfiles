;;; functions.el --- My custom Emacs Lisp functions
; Edit this config

;;; Commentary:
;; This file contains both interactive and non-interactive functions
;; that are key bound to keys in the keybindings.el file

;;; Code:

(defun my/edit-emacs-configuration ()
  "Edit Emacs configuration."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun my/edit-emacs-settings ()
  "Open common settings file."
  (interactive)
  (find-file "~/.emacs.d/rajasegar/settings.el"))

(defun my/edit-emacs-packages ()
  "Open  packages file."
  (interactive)
  (find-file "~/.emacs.d/rajasegar/packages.el"))

(defun my/edit-emacs-keybindings ()
  "Open  keybindings file."
  (interactive)
  (find-file "~/.emacs.d/rajasegar/keybindings.el"))

(defun my/edit-emacs-functions ()
  "Open  keybindings file."
  (interactive)
  (find-file "~/.emacs.d/rajasegar/functions.el"))

(defun my/open-eglot-config ()
  "Open eglot config file."
  (interactive)
  (find-file "~/.emacs.d/rajasegar/eglot-config.el"))

(defun my/edit-emms-config ()
  "Open emms config file."
  (interactive)
  (find-file "~/.emacs.d/rajasegar/music.el"))


(defun my/open-emacs-config-folder ()
  "Open Emacs config folder."
  (interactive)
  (dired "~/.emacs.d/rajasegar"))

(defun my/toggle-buffers ()
  "Toggle buffers."
  (interactive)
  (switch-to-buffer nil))

(defun my/switch-to-dashboard ()
 "Switch to dashboard buffer."
  (interactive)
  (switch-to-buffer dashboard-buffer-name))

(defun my/play-favs-folder ()
  "Play the Favs directory in EMMS."
  (interactive)
  (emms-play-directory-tree "~/Music/Favs"))

(defun my/play-college-folder ()
  "Play the College directory in EMMS."
  (interactive)
  (emms-play-directory-tree "~/Music/College"))

(defun my/play-latest-folder ()
  "Play the Latest directory in EMMS."
  (interactive)
  (emms-play-directory-tree "~/Music/Latest"))

(defun my/open-hackernews ()
  "OPen hacker news website in eww."
  (interactive)
  (eww "hackernews.com"))



(defun get-projects ()
  "Get the list of recently visited projects."
 (with-temp-buffer
    (insert-file-contents "~/.emacs.d/projects")
    (let* ((projects (read (current-buffer)))
          (props '()))
      (loop for (p) in projects do
            (add-to-list 'props p))
      props)))

(defun my/find-projects-function (str pred _)
  "Callback function for `ivy-read' for projects list.
Argument STR string.
Argument PRED predicate."
  (let* ((props (get-projects))
        (strs (cl-mapcar (lambda (p) (car (last (split-string p "/" t)))) props)))
    (cl-mapcar (lambda (s p) (propertize s 'property p))
               strs
               props)))

  

(defun my/find-projects ()
  "Find the projects."
  (interactive)
  (tab-bar-new-tab)
  (ivy-read "Find projects: "
            #'my/find-projects-function
            :action (lambda (x)
                      (project-switch-project (get-text-property 0 'property x))
                      (tab-bar-rename-tab x))))

(defun my/open-project ()
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


(defun my/open-new-pull-request ()
  "Open new pull request url for current branch in browser."
  (interactive)
  (shell-command (concat "open " (github-repository-url) "/pull/new/" (car (vc-git-branches)))))

(defun my/compare-git-branches ()
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


(defun my/create-prodigy-service (&optional package-manager)
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

(defun my/stage-file-in-current-line ()
  "Magit Stage the file name in the current line."
  (interactive)
  (let ((filename (string-trim (buffer-substring-no-properties (+ 2 (line-beginning-position)) (line-end-position)))))
    (message "Staging file: %s" filename)
    (magit-stage-file filename)
    (message "File staged successfully: %s !!" filename)))

(defun my/magit-stash-untracked ()
  "Stash include untracked files using magit."
  (interactive)
  (magit-stash-both (read-string "Enter stash name: ") t))

(defun my-project-root ()
  "Find the project root."
    (locate-dominating-file (file-name-directory (buffer-file-name)) "package.json"))


(defun my/counsel-rg-word ()
  "Search word under cursor using `counsel-rg'."
  (interactive)
  (counsel-rg (word-at-point)))

(defun my/run-rcup ()
  "Run rcup -v to update dotfiles."
  (interactive)
  (async-shell-command "rcup -v")
  (switch-to-buffer-other-window "*Async Shell Command*"))


(defun my/create-gist ()
  "Create gist from current buffer."
  (interactive)
  (eshell-command (concat "gh gist create "  (file-name-base (buffer-file-name)) "." (file-name-extension (buffer-file-name)) )))

(defun my/update-wallpaper ()
  "Fetch a new wallpaper from Upsplash and update it."
  (interactive)
  (url-copy-file "https://picsum.photos/1920/1080/?random" "~/Pictures/random/wallpaper.jpg" t)
  (mapcar (lambda (str)
            (async-shell-command (format "xfconf-query -c xfce4-desktop -p \"%s\" -s ~/Pictures/random/wallpaper.jpg" str)))
          (string-split 
           (shell-command-to-string "xfconf-query -c xfce4-desktop -p /backdrop -l | grep last-image"))))



(defun my/apt-get-install ()
  "Read a package name from minibuffer and install it with apt-get."
  (interactive)
  (eshell-command (concat "sudo apt-get -y install " (read-string "Enter the package name: "))))

(defun my/open-project-in-github ()
  "Open the github url of the current project"
  (interactive)
  (shell-command (concat "open " (github-repository-url))))

(defun my/open-github-pull-requests ()
  "Open the github pull requests url of the current project"
  (interactive)
  (shell-command (concat "open " (github-repository-url) "/pulls")))

(defun my/open-github-issues ()
  "Open the github issues url of the current project"
  (interactive)
  (shell-command (concat "open " (github-repository-url) "/issues")))

(defun my/dired-sort-size ()
  "Dired sort by size."
  (interactive)
  (dired-sort-other  "-lS"))

(defun my/dired-sort-size-reverse ()
  "Dired sort by size."
  (interactive)
  (dired-sort-other  "-lrS"))

(defun my/eval-print-last-sexp-no-truncation ()
  "Eval sexp and print without truncating the output"
  (interactive)
  (eval-print-last-sexp 0))

(defun my/copy-buffer ()
  (interactive)
  ;; (mark-whole-buffer)
  (copy-region-as-kill (point-min) (point-max)))

(defun my/copy-line ()
  (interactive)
  (copy-region-as-kill (point-at-bol) (point-at-eol)))

(defun my/delete-current-line()
  "Delete the current line under the cursor"
  (interactive)
  (kill-region (point-at-bol) (point-at-eol)))

(defun my/duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun my/kill-to-eof ()
  "Kill from the current cursor position to end of file"
  (interactive)
  (kill-region (point) (point-max)))

(defun my/search-word-in-project ()
  "Search the word under the cursor in  project"
  (interactive)
  (project-find-regexp (thing-at-point 'word t)))

(defun my/plantuml-preview ()
  "Preview plantuml"
  (interactive)
  (shell-command (concat "java -jar ~/plantuml.jar " (buffer-file-name)))
  (find-file (string-replace ".pum" ".png" (buffer-file-name))))

(defun my/compile-c ()
  "Compile c programs with cc"
  (interactive)
  (compile (format "cc -o %s %s" (file-name-base (buffer-file-name)) (buffer-file-name))))

(defun my/run-c ()
  "Run the compiled C program"
  (interactive)
  (shell-command (format "./%s" (file-name-base (buffer-file-name)))))


(defun my/indent-whole-buffer ()
  "Mark the current buffer entirely and indent it"
  (interactive)
  (indent-region (point-min) (point-max))
  )

(defun my/run-with-node ()
  "Run the current js file with Node"
  (interactive)
  (async-shell-command (format "node %s" (buffer-file-name))))

(defun my/run-with-tsc ()
  "Run the current TS file with Typescript"
  (interactive)
  (async-shell-command (format "tsc %s && node %s" (buffer-file-name) (concat (file-name-base (buffer-file-name)) ".js"))))

(defun my/make-temp-el ()
  "Create a temporary Emacs Lisp file to evaluate"
  (interactive)
  (let ((temp-dir (make-temp-file "emacs-lisp-" t )))
    (find-file (concat temp-dir "/temp.el"))))

(defun my/wordpress-rider-tshirt ()
  "Run rcup -v to update dotfiles."
  (interactive)
  (async-shell-command "node wordpress/rider-tshirt.js")
  (switch-to-buffer-other-window "*Async Shell Command*"))

(defun hex-to-rgb (hex)
  "Convert HEX color to RGB."
  (let* ((hex (if (string-prefix-p "#" hex) (substring hex 1) hex))
         (r (string-to-number (substring hex 0 2) 16))
         (g (string-to-number (substring hex 2 4) 16))
         (b (string-to-number (substring hex 4 6) 16)))
    (format "%d %d %d" r g b)))

(defun replace-word-at-point (new-word)
  "Replace the word at point with NEW-WORD, preserving case pattern."
  (save-match-data
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (if bounds
          (progn
            (set-match-data (list (car bounds) (cdr bounds)))
            (replace-match new-word))
        (message "No word at point")))))   

(defun my/replace-hex-with-rgb ()
  "Replace the HEX value with RGB at cursor position"
  (interactive)
  (replace-word-at-point (hex-to-rgb (word-at-point))))

(defun my/run-with-gimp ()
  "Run current buffer as script for gimp"
  (interactive)
  (async-shell-command
   (format
    "gimp-console --batch-interpreter=plug-in-script-fu-eval --batch='%s' --batch='(gimp-quit 0)'"
    (buffer-substring-no-properties (point-min) (point-max))
    )))

(defun my/run-scheme-with-gimp ()
  "Load and execute a scheme file (current buffer) with gimp"
  (interactive)
  (async-shell-command
   (format
    "gimp-console --batch-interpreter=plug-in-script-fu-eval --batch='(load \"%s\")' --batch='(gimp-quit 0)'"
    (buffer-file-name)
    )))


;; gimp-console --batch-interpreter=plug-in-script-fu-eval --batch="(gimp-message \"foo\")"
;; gimp-console --batch-interpreter=plug-in-script-fu-eval --batch="(load \"my.scm\")"



(provide 'functions)

;;; functions.el ends here
