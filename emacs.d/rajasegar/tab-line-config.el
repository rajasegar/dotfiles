(setq tab-line-new-button-show nil)  ;; do not show add-new button
(setq tab-line-close-button-show nil)  ;; do not show close button

;; Show tab line 
(global-tab-line-mode)

; tab color settings
(set-face-attribute 'tab-line nil ;; background behind tabs
      :background (doom-color 'bg)
      :foreground (doom-color 'green) :distant-foreground (doom-color 'teal)
      :height 1.0 :box nil)
(set-face-attribute 'tab-line-tab nil ;; active tab in another window
      :inherit 'tab-line
      :foreground (doom-color 'green) :background (doom-color 'bg) :box nil)
(set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
      :background (doom-color 'bg-alt) :foreground "yellow" :box nil)
(set-face-attribute 'tab-line-tab-inactive nil ;; inactive tab
      :background (doom-color 'bg-alt) :foreground "black" :box nil)
(set-face-attribute 'tab-line-highlight nil ;; mouseover
      :background "white" :foreground 'unspecified)

;; Tab bar
;; Inherit the face of `doom-modeline-panel` for better appearance
(set-face-attribute 'tab-bar-tab nil :inherit 'doom-modeline-panel :foreground nil :background nil)

(defun random-element-of-list (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))


;; Totally customize the format of the tab bar name
(defun my/tab-bar-format (tab i)
  (propertize
   (format
    (concat
      (if (eq (car tab) 'current-tab)
          " 🚀 " (random-element-of-list '(" ⚽ " " 🏀 " " 🏈 " " ⚾ " " 🥎 " " 🎾 " " 🏐 " " 🏉 ")))
      "%s ")
    (alist-get 'name tab))
   'face (list (append
                  '(:foreground "#FFFFFF")
                  (if (eq (car tab) 'current-tab)
                      '(:foreground "black" :background "white")
                      '())))))

;; Replace the default tab bar function
(setq tab-bar-tab-name-format-function #'my/tab-bar-format)

(defun my/tab-bar-tab-name-function ()
  (let ((project (project-current)))
    (if project
        (project-root project)
        (tab-bar-tab-name-current))))

(setq tab-bar-tab-name-function #'my/tab-bar-tab-name-function)

;; Only show the tab bar if there are 2 or more tabs
(setq tab-bar-show 1)

;; (defun my/tab-bar-string () "HELLO")

;; Customize the tab bar format to add the global mode line string
;; (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global))

;; (add-to-list 'global-mode-string "HELLO")

;; Make sure mode line text in the tab bar can be read
(set-face-attribute 'tab-bar nil :foreground "#FFFFFF")

(defun my/project-create-tab ()
  (interactive)
  (tab-bar-new-tab)
  (magit-status))

(setq project-switch-commands #'my/project-create-tab)

(defun my/switch-to-tab-buffer ()
  (interactive)
  (if (project-current)
      (call-interactively #'project-switch-to-buffer)
    (call-interactively #'switch-to-buffer)))

(global-set-key (kbd "C-x b") #'my/switch-to-tab-buffer)

;; Turn on tab bar mode after startup
(tab-bar-mode 1)
