; Edit this config
(defun edit-emacs-configuration ()
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

(defun rajasegar/play-favs-folder ()
  "Play the Favs directory in EMMS"
  (interactive)
  (emms-play-directory "~/Music/Favs"))

(defun rajasegar/play-college-folder ()
  "Play the College directory in EMMS"
  (interactive)
  (emms-play-directory "~/Music/College"))

(defun rajasegar/play-latest-folder ()
  "Play the 2023 directory in EMMS"
  (interactive)
  (emms-play-directory-tree "~/Music/2023"))

(defun open-new-eshell ()
  "Open new shell instance everytime"
  (interactive)
  (eshell 'N))

(defun rajasegar/open-hackernews ()
  "OPen hacker news website in eww"
  (interactive)
  (eww "hackernews.com"))
