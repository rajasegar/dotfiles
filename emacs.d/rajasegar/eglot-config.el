;; LSP with eglot

(add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-ts-mode))

;; (put 'typescript-ts-mode 'eglot-language-id "typescriptreact")
;; (put 'js-mode 'eglot-language-id "javascript")

(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'web-mode-hook 'eglot-ensure)
(add-hook 'scheme-mode-hook 'eglot-ensure)

(defun jsonrpc--log-event (connection message &optional type))


(with-eval-after-load 'eglot
;; No event buffers, disable providers cause a lot of hover traffic. Shutdown unused servers.
  (setq eglot-events-buffer-size 0
        eglot-autoshutdown t)

    ;; (add-to-list 'eglot-server-programs
             ;; '((scheme-mode) . ("/home/rajasegar/Downloads/scheme-langserver-x86_64-linux-glibc" )))


  )



