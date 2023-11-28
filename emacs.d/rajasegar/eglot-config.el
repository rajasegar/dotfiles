;; LSP with eglot

(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'hbs-mode-hook 'eglot-ensure)

;; Handlebars mode
(define-derived-mode hbs-mode web-mode "Handlebars mode" "Major mode for handlebars")
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . hbs-mode))


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               (if (string-equal system-type "darwin")
                   '(hbs-mode . ("node" "/Users/rajasegarchandran/www/ember-language-server/lib/start-server.js" "--stdio"))
                 '(hbs-mode . ("node" "/home/boot/Public/www/ember-language-server/lib/start-server.js" "--stdio")))
               ;; '(js2-mode . ("node" "/home/boot/Public/www/bun-lsp/out/server.js" "--stdio"))
               ))

(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (add-to-list 'company-backends
                                                  '(company-capf :with company-yasnippet))))

