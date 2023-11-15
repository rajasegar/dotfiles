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
