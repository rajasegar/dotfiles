(defvar chatgpt-ai-base-url "platforms-freddy-staging-francecentral-ai-stage01")
(defvar chatgpt-ai-deployment "gpt-35-turbo")
(defvar chatgpt-ai-api-version "2023-03-15-preview")

(defun chatgpt-shell-key-auth-source (&optional base-url)
  (if-let ((auth-info (auth-source-search :max 1
                                          :host (concat chatgpt-ai-base-url ".openai.azure.com"))))
      (funcall (plist-get (car auth-info) :secret))
    (error "ChatGPT API key not found in auth-source")))


(use-package chatgpt-shell
  :ensure t)

(setq chatgpt-shell-api-url-base (concat "https://" chatgpt-ai-base-url ".openai.azure.com"))
(setq chatgpt-shell-api-url-path (concat "/openai/deployments/" chatgpt-ai-deployment "/chat/completions?api-version=" chatgpt-ai-api-version))
(setq chatgpt-shell-auth-header (lambda () (format "api-key: %s" (chatgpt-shell-key-auth-source))))
(setq chatgpt-shell-openai-key (chatgpt-shell-key-auth-source))
