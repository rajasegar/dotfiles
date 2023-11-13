;; LSP with eglot
(add-hook 'js2-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'hbs-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               ;; '(hbs-mode . ("node" "/Users/rajasegarchandran/www/ember-language-server/lib/start-server.js" "--stdio"))
               '(hbs-mode . ("node" "/home/boot/Public/www/ember-language-server/lib/start-server.js" "--stdio"))
               '(js2-mode . ("node" "/home/boot/Public/www/bun-lsp/out/server.js" "--stdio"))
               ))

(defun jsonrpc--log-event (_connection _message &optional type)
  "A NOOP just for issue 61"
  nil)

(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (add-to-list 'company-backends
                                                  '(company-capf :with company-yasnippet))))


