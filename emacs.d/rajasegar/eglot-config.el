;; LSP with eglot

(add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode))

(put 'typescript-ts-mode 'eglot-language-id "typescriptreact")
(put 'js-mode 'eglot-language-id "javascript")

(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'hbs-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'web-mode-hook 'eglot-ensure)

(defun jsonrpc--log-event (connection message &optional type))

;; Handlebars mode
(define-derived-mode hbs-mode web-mode "Handlebars mode" "Major mode for handlebars")
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . hbs-mode))

(with-eval-after-load 'eglot
;; No event buffers, disable providers cause a lot of hover traffic. Shutdown unused servers.
  (setq eglot-events-buffer-size 0
        ;; eglot-ignored-server-capabilities '(
                                            ;; :hoverProvider
                                            ;; :documentHighlightProvider
                                            ;; :inlayHintProvider)
        eglot-autoshutdown t)

  (add-to-list 'eglot-server-programs
               (if (string-equal system-type "darwin")
                   '(hbs-mode . ("node" "/Users/rajasegarchandran/www/ember-language-server/lib/start-server.js" "--stdio"))
                 '(hbs-mode . ("node" "/home/boot/Public/www/ember-language-server/lib/start-server.js" "--stdio")))
               ;; '(js2-mode . ("node" "/home/boot/Public/www/bun-lsp/out/server.js" "--stdio"))
               ))



