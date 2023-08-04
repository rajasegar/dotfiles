; Edit this config
(defun edit-emacs-configuration ()
  "Edit Emacs configuration."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun toggle-buffers ()
  "Toggle buffers."
  (interactive)
  (switch-to-buffer nil))

(defun switch-to-dashboard ()
 "Switch to dashboard buffer."
  (interactive)
  (switch-to-buffer dashboard-buffer-name))

(defun switch-git-personal ()
  "Switch to personal Github profile."
  (interactive)
  (shell-command "ssh-add -D && ssh-add ~/.ssh/id_rsa && ssh -T git@github.com"))

(defun switch-git-work ()
  "Switch to work Github profile."
  (interactive)
  (shell-command "ssh-add -D && ssh-add ~/.ssh/id_ed25519 && ssh -T git@github.com"))

(defun play-favs-folder ()
  "Play the Favs directory in EMMS"
  (interactive)
  (emms-play-directory "~/Music/Favs"))

(defun play-college-folder ()
  "Play the College directory in EMMS"
  (interactive)
  (emms-play-directory "~/Music/College"))

(defun play-latest-folder ()
  "Play the 2023 directory in EMMS"
  (interactive)
  (emms-play-directory-tree "~/Music/2023"))

(defun open-new-eshell ()
  "Open new shell instance everytime"
  (interactive)
  (eshell 'N))

