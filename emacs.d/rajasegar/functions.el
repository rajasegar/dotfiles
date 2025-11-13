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
  "Play the 2023 directory in EMMS."
  (interactive)
  (emms-play-directory-tree "~/Music/2023"))

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
  (async-shell-command "xfconf-query -c xfce4-desktop -p /backdrop/screen0/monitorHDMI-1/workspace0/last-image -s ~/Pictures/random/wallpaper.jpg")
  )

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


(defun my/startup ()
  "Start apps in Mac in a bunch from Emacs"
  (interactive)
  (shell-command "open -a Firefox.app")
  (shell-command "open -a Slack.app")
    (shell-command "open -a 'Google Chrome.app'"))


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

(defun my/insta-prepare-images ()
  "Resize and crop images to Insta reel format 1080x1920 px"
  (interactive)
  (let ((folder (read-directory-name "Choose directory:" "~/Videos/test"))
        (ext (read-string "Enter extension: " "jpg")))
  (async-shell-command 
   (format "gimp -i -c -b '(script-fu-resize-insta-reel \"*.%s\"  \"%s\")' -b '(gimp-quit 0)'  --batch-interpreter plug-in-script-fu-eval" ext  folder))))


(defun my/ffmpeg-create-video ()
  "Create a video from images"
  (interactive)
  (let ((folder (read-directory-name "Choose directory:" "~/Videos"))
        (interval (read-number "Enter time interval: " 5))
        (ext (read-string "Enter extension: " "jpg")))
    (setq default-directory folder)
  (async-shell-command (format "ffmpeg -r 1/%d -pattern_type glob -i '*.%s' -c:v libx264 -r 30 -pix_fmt yuv420p input.mp4" interval ext))))

(defun my/ffmpeg-add-audio ()
  "Add audio to input videos"
  (interactive)
  (let ((video (read-file-name "Choose video file: "))
        (audio (random-element-of-list (directory-files  "~/Zazzle/Audio-Clips" t "\\`[^.]")))
        ;; (audio (read-file-name "Choose audio file: " "~/Zazzle/Audio-Clips"))
        )
    (setq default-directory (file-name-directory video))
  (async-shell-command (format "ffmpeg -i %s -i %s -shortest -c:v copy -map 0:v -map 1:a output.mp4" video audio))))

(defun delete-all-files-in-directory (dir)
  "Delete all files in the specified DIR."
  (let ((files (directory-files dir t "\\`[^.]")))  ; Exclude '.' and '..'
    (dolist (file files)
      (when (file-regular-p file)  ; Check if it's a regular file
        (delete-file file)))))  ; Delete the file

(defun snake-case (str)
(string-join (mapcar #'downcase (split-string str)) "-"))

(defun copy-reel-files (mockup)
  (mapcar (lambda (file)
            (let ((name (file-name-base file))
                  (ext (file-name-extension file)))
              (if (or (string-search "Generated" name) (string-search "generated" name))
                  (copy-file file (format "~/Videos/insta-reel/001.%s" ext)))
              (if (string-search "person-3" name)
                  (copy-file file (format "~/Videos/insta-reel/002.%s" ext)))
              (if (string-search "person-4" name)
                  (copy-file file (format "~/Videos/insta-reel/003.%s" ext)))
              (if (string-search "hanging" name)
                  (copy-file file (format "~/Videos/insta-reel/004.%s" ext)))
              (if (string-search "front" name)
                  (copy-file file (format "~/Videos/insta-reel/005.%s" ext)))
              (if (string-search "person-2" name)
                  (copy-file file (format "~/Videos/insta-reel/006.%s" ext)))
            ))
          
          (seq-filter (lambda (name) (not (or
                                           (string-search "back" name)
                                           (string-search "person-1" name)
                                           (string-search "person-5" name))))
                      (directory-files  (format "~/Printify/Mockups/%s" mockup) t "\\`[^.]"))))





(defun my/create-insta-reel ()
  "Create instagram reel from images"
  (interactive)

  (let ((folder  "/home/rajasegar/Videos/insta-reel/" )
        (audio (random-element-of-list (directory-files  "~/Zazzle/Audio-Clips" t "\\`[^.]")))
        (mockup (format "%s Tote Bag" (read-string "Mockup name: ")))
        (reel-name (format "Totefy-Reel-%s.mp4" (format-time-string "%d-%b-%Y-%H-%M-%S"))))
    (setq default-directory folder)
    (shell-command-queue-clear)

    (delete-all-files-in-directory folder)

    (copy-reel-files mockup)

    (copy-file "~/GIMP/follow-us-instagram-1920.png" (format "%s/007.png" folder))

    (shell-command-queue-add
     "Preparing Images"
     (format "gimp -i -c -b '(script-fu-batch-export-png-to-jpg \"*.png\"  \"%s\")' -b '(gimp-quit 0)'  --batch-interpreter plug-in-script-fu-eval"  folder))

    (shell-command-queue-add
     "Cropping Images"
     (format "gimp -i -c -b '(script-fu-resize-insta-reel \"*.jpg\"  \"%s\")' -b '(gimp-quit 0)'  --batch-interpreter plug-in-script-fu-eval"  folder))

    (shell-command-queue-add
     "Change directory"
     "cd /home/rajasegar/Videos/insta-reel")

    (shell-command-queue-add
     "Creating Input Video"
     "ffmpeg -r 1/5 -pattern_type glob -i '*.jpg' -c:v libx264 -r 30 -pix_fmt yuv420p input.mp4")

    (shell-command-queue-add
     "Adding audio"
     (format "ffmpeg -i input.mp4 -i %s -shortest -c:v copy -map 0:v -map 1:a %s" audio reel-name))

    (shell-command-queue-add
     "Copy reel to the mockup folder"
     (format "cp ~/Videos/insta-reel/%s '/home/rajasegar/Printify/Mockups/%s/%s'" reel-name mockup reel-name))

    (shell-command-queue-run)
    (pop-to-buffer "*shell-command-queue-output*")

    ))

(defun my/run-printify-task ()
  "Run the printify task in node to create tote bags and download mockups"
  (interactive)
  (setq default-directory "~/www/woocommerce")
  (async-shell-command "node printify.js"))

(defun my/run-woocommerce-task ()
  "Run the printify task in node to create tote bags and download mockups"
  (interactive)
  (setq default-directory "~/www/woocommerce")
  (async-shell-command "node woocom.js"))

(defun my/run-upload-task ()
  "Copy and prepare images for wordpress upload"
  (interactive)
  (setq default-directory "~/www/woocommerce")
  (async-shell-command "node upload.js"))

(defun my/run-download-task ()
  "Copy and prepare images for wordpress upload"
  (interactive)
  (setq default-directory "~/www/woocommerce")
  (async-shell-command "node download-mockups.js"))

(defun my/whatsapp-story ()
  "Create Whatsapp/Instagram story from images"
  (interactive)

  (let ((folder  "/home/rajasegar/Videos/whatsapp-story/" )
        (audio (random-element-of-list (directory-files  "~/Zazzle/Audio-Clips" t "\\`[^.]")))
        (reel-name (format "Whatsapp-Story-%s.mp4" (format-time-string "%d-%b-%Y-%H-%M-%S")))
        )
    (setq default-directory folder)
    (shell-command-queue-clear)

    (shell-command-queue-add
     "Preparing Images"
     (format "gimp -i -c -b '(script-fu-batch-export-png-to-jpg \"*.png\"  \"%s\")' -b '(gimp-quit 0)'  --batch-interpreter plug-in-script-fu-eval"  folder))

    (shell-command-queue-add
     "Cropping Images"
     (format "gimp -i -c -b '(script-fu-resize-insta-reel \"*.jpg\"  \"%s\")' -b '(gimp-quit 0)'  --batch-interpreter plug-in-script-fu-eval"  folder))

    (shell-command-queue-add
     "Creating Input Video"
     "ffmpeg -r 1/4 -pattern_type glob -i '*.jpg' -c:v libx264 -r 30 -pix_fmt yuv420p input.mp4")

    (shell-command-queue-add
     "Adding audio"
     (format "ffmpeg -i input.mp4 -i %s -shortest -c:v copy -map 0:v -map 1:a %s" audio reel-name))

    (shell-command-queue-add
     "Copy reel to the Stories folder"
     (format "cp ~/Videos/whatsapp-story/%s '/home/rajasegar/Zazzle/Whatsapp-Story/%s'" reel-name reel-name))


    (shell-command-queue-run)
    (pop-to-buffer "*shell-command-queue-output*")

    ))

(provide 'functions)

;;; functions.el ends here
