;; LSP
(setq lsp-keymap-prefix "C-c l")
(use-package lsp-mode
  :ensure t
  ;; :init
  ;; (setq lsp-keymap-prefix "C-c l")
  :hook ((js-mode . lsp-deferred)
         (html-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         ;(svelte-mode . lsp-deferred)
         ;(hbs-mode . lsp-deferred)
         )
  :commands lsp-deferred
  :config
  (lsp-enable-which-key-integration t)
  )

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))


(setq lsp-enable-file-watchers nil)
;; LSP performance
(setq gc-cons-threshold (* 100 1000 1000)) ;; 100 mb
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Disable warning for no matched clients
(setq lsp-warn-no-matched-clients nil)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))
(setq lsp-ui-doc-enable nil)
(setq lsp-lens-enable nil)

;; Handlebars mode
(define-derived-mode hbs-mode web-mode "Handlebars mode" "Major mode for handlebars")


(add-to-list 'auto-mode-alist '("\\.hbs\\'" . hbs-mode))


;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration
;;                '(hbs-mode . "hbs"))
;;   (lsp-register-client
;;    ;; Git clone language server from https://github.com/lifeart/ember-language-server/tree/component-context-info-origin
;;    ;; And build it
;;    (make-lsp-client :new-connection (lsp-stdio-connection (list "node" (expand-file-name "~/Public/www/ember-language-server/lib/start-server.js") "--stdio"))
;;                     :activation-fn (lsp-activate-on "hbs")
;;                     :server-id 'ember-language-server))
;;   ;; (lsp-register-client
;;   ;;   (make-lsp-client :new-connection (lsp-stdio-connection (list "node" (expand-file-name "~/Public/www/bun-lsp/out/server.js") "--stdio"))
;;   ;;                    :activation-fn (lsp-activate-on "javascript")
;;   ;;                    :server-id 'bun-lsp-server))
;;   )
