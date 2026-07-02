;;; settings.el --- This is the settings config file

;;; Commentary:
;; All the Emacs configuration settings 

;;; Code:
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq delete-old-versions -1 )
(setq inhibit-startup-screen t )
(setq ring-bell-function 'ignore )
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq initial-scratch-message "")
(setq word-wrap t)

;; don't create lock files
(setq create-lockfiles nil)

;; start in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Hide tab bar since we are using mode line to show the active tab
(setq tab-bar-close-button-show nil)
(setq tab-bar-tab-hints t)                 ;; show tab numbers

;; Tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq js-indent-level 2)

;; Eshell case insensitive glob
(setq eshell-cmpl-ignore-case t)


;; Enable copy paste
(setq select-enable-clipboard t)

(global-set-key (kbd "C-S-v") #'clipboard-yank)

;; https://github.com/danielmai/.emacs.d/blob/master/config.org
(defalias 'yes-or-no-p 'y-or-n-p)
;; Ask yes or no instead of y or n
(setq use-short-answers t)
(blink-cursor-mode -1)
(show-paren-mode t)

;; (setq-default mode-line-format nil)

(toggle-word-wrap)
(global-auto-revert-mode t)
(global-display-line-numbers-mode 1)
(electric-pair-mode)

(global-display-line-numbers-mode)
;; Set relative line numbers
(setq display-line-numbers-type 'relative)


;; Highlight current line
(global-hl-line-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook
                org-present-mode-hook
                dashboard-mode-hook
                nov-mode-hook
                eww-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Font
(when (string-equal system-type "darwin")
    (add-to-list 'default-frame-alist '(font . "Monaco-16" ))
    (set-face-attribute 'default t :font "Monaco-16" ))

(when (string-equal system-type  "gnu/linux")
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11" ))
    (set-face-attribute 'default t :font "DejaVu Sans Mono-11" ))

(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

(setq org-clock-sound "~/.emacs.d/bell.wav")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/rajasegar.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("e" "Emacs Todo" entry (file+headline "~/Dropbox/org/rajasegar.org" "Emacs")
         "* TODO %?\n  %i\n  %a")
        ("i" "Investing Todo" entry (file+headline "~/Dropbox/org/investing.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("v" "Inkscape Todo" entry (file+headline "~/Dropbox/org/inkscape.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("g" "Gimp Todo" entry (file+headline "~/Dropbox/org/gimp.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n  %i")))

(setq nov-text-width t)
(setq visual-fill-column-center-text t)


;; Enable visual line and fill column for some modes
(dolist (mode '(nov-mode-hook
                eww-mode-hook))
  (add-hook mode (lambda ()
                   (visual-line-mode 1)
                   (visual-fill-column-mode 1))))

(setq org-hide-emphasis-markers t)
(setq org-emphasis-alist
      (quote (
              ("*" (bold :foreground "cyan"))
              ("/" (italic :foreground-color "red")
              ("_" underline)
              ("=" (:foreground "yellow" ))
              ("~" org-verbatim verbatim)
              ("+" (:strike-through t))
              ))))
;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Turn on repeat mode
(repeat-mode 1)

;; Turn on abbrev mode globally
(abbrev-mode 1)

;; (setq next-line-add-newlines t)


;; Ediff tweaks
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(defun raja-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'raja-ediff-hook)

;; Modus Themes
;; (setq modus-themes-fringes nil
      ;; modus-themes-mode-line '(accented)
      ;; modus-themes-subtle-line-numbers t)
;; (load-theme 'modus-vivendi)

;; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Dired human listing
(setq dired-listing-switches "-lFh")

;; Newsticker urls
(setq newsticker-url-list
      '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
        ("OpenAI News"	"https://openai.com/news/rss.xml")
        ("Hugging Face Blog"	"https://huggingface.co/blog/feed.xml")
        ("MIT Technology Review (AI)"	"https://www.technologyreview.com/topic/artificial-intelligence/feed/")
        ("Google AI Blog"	"https://blog.google/technology/ai/rss/")
        ("MarkTechPost"	"https://www.marktechpost.com/feed/")
        ("arXiv cs.AI"	"https://rss.arxiv.org/rss/cs.AI")
        ("GitHub curated AI RSS lists"	"https://github.com/vishalshar/awesome_ML_AI_RSS_feed")
        ("Reddit AI RSS curation thread"	"https://www.reddit.com/r/rss/comments/1ezcwdh/large_collection_of_rss_feeds_in_the_ai_space/")
        ))


(provide 'settings)

;;; settings.el ends here
