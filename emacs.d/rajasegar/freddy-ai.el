(defvar freddy-ai-base-url "https://platforms-freddy-staging-francecentral-ai-stage01.openai.azure.com/openai/deployments/gpt-35-turbo/chat/completions?api-version=2023-03-15-preview")


(defvar freddy-ai-directives
  '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
    (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
    (writing . "You are a large language model and a writing assistant. Respond concisely.")
    (chat . "You are a large language model and a conversation partner. Respond concisely.")))


(defun freddy-ai/request-sync (freddy-ai-prompt directive)
  "Sends a request to Freddy-Ai API and return the response.
Argument FREDDY-AI-PROMPT is the prompt to send to the API."
  (when (null freddy-ai-api-key)
    (error "Freddy-Ai API key is not set"))

  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("api-key" . ,(format "%s" freddy-ai-api-key))))
         (url-request-data
          (json-encode `((messages .
                                   [((role . "system")
                                     (content . directive))
                                    ((role . "user")
                                     (content . ,freddy-ai-prompt))]))

                       ))
         (buffer (url-retrieve-synchronously freddy-ai-base-url nil 'silent))
         response)

    ;; (message "Sending request to Freddy-Ai API " )

    (if buffer
        (with-current-buffer buffer
          (goto-char url-http-end-of-headers)
          (condition-case freddy-ai-err
              (progn
                (setq response (json-read))
                (if (assoc 'error response)
                    (error (cdr (assoc 'message (cdr (assoc 'error response)))))
                  response))
            (error (error "Error while parsing Freddy-Ai API response: %s"
                          (error-message-string freddy-ai-err)))))
      (error "Failed to send request to Freddy-Ai API"))
    ))

(defun get-content (response)
  "Get the content value from the response"
  ;; (message response)
  (cdr (assoc 'content (elt (aref (cdr (assoc 'choices response)) 0) 2))))


(defun freddy-ai/rephrase-from-region ()
  "Rephrase the text from the active region"
  (interactive)
  (let* ((prompt (concat "Rephrase: " (buffer-substring-no-properties (region-beginning) (region-end))))
        (buffer "*Freddy-AI*"))
    (with-temp-buffer (get-buffer-create buffer)
                      (switch-to-buffer-other-window buffer)
                      (insert (get-content (freddy-ai/chat prompt))))))

(defun freddy-ai/write-code ()
  "Generate code"
  (interactive)
  (let* ((prompt (read-string "Enter your question: "))
        (buffer "*Freddy-AI*"))
                       (freddy-ai/request prompt (cdr (assoc 'programming freddy-ai-directives)))))

(defun freddy-ai/explain-code ()
  "Explain code in the current buffer"
  (interactive)
  (let* ((prompt (concat "Explain the following code:\n"
                         (buffer-substring-no-properties (point-min) (point-max))))
        (buffer "*Freddy-AI*"))
                      (freddy-ai/request prompt (cdr (assoc 'programming freddy-ai-directives)))))

(defun freddy-ai/write-tests ()
  "Explain code in the current buffer"
  (interactive)
  (let* ((prompt (concat "Write unit tests for the following code:\n"
                         (buffer-substring-no-properties (point-min) (point-max))))
        (buffer "*Freddy-AI*"))
                       (freddy-ai/code prompt)))

(defun freddy-ai/code (prompt)
"An assistant to perorm code related activities"
  (freddy-ai/request prompt (cdr (assoc 'programming freddy-ai-directives))))

(defun freddy-ai/chat (prompt)
"Send the prompt text from the minibuffer"
  (freddy-ai/request prompt (cdr (assoc 'default freddy-ai-directives))))


(defun freddy-ai/prompt ()
  "Send the prompt text from the minibuffer"
  (interactive)
  (let ((prompt (read-string "Enter your question: ")))
     (freddy-ai/chat prompt)))


(defun freddy-ai/prompt-from-line ()
  "Send the prompt text from the current line"
  (interactive)
  (let ((prompt (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (freddy-ai/chat prompt)))

(defun freddy-ai/prompt-from-region ()
  "Send the prompt text from the active region"
  (interactive)
  (let ((prompt (buffer-substring-no-properties (region-beginning) (region-end))))
     (freddy-ai/chat prompt)))



(defun freddy-ai/key-auth-source (&optional base-url)
  (if-let ((auth-info (auth-source-search :max 1
                                          :host "platforms-freddy-staging-francecentral-ai-stage01.openai.azure.com")))
      (funcall (plist-get (car auth-info) :secret))
    (error "Freddy-Ai API key not found in auth-source")))


(defun freddy-ai/handle-response (status)
  "Callback function to handle the URL response."
  (if (plist-get status :error)
      (message "Error retrieving URL: %s" (plist-get status :error))
    (goto-char (point-min))
    (search-forward "\n\n")
    (let ((json-text (buffer-substring (point) (point-max))))
      (with-current-buffer (generate-new-buffer "*Freddy-AI*")
        (insert (get-content (json-read-from-string json-text)))
        (markdown-mode)
        (pop-to-buffer (current-buffer))))))


(defun freddy-ai/request (freddy-ai-prompt directive)
  "Sends a request to Freddy-Ai API and return the response. "
  (when (null freddy-ai-api-key)
    (error "Freddy-Ai API key is not set"))

  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("api-key" . ,(format "%s" freddy-ai-api-key))))
         (url-request-data
          (json-encode `((messages .
                                   [((role . "system")
                                     (content . directive))
                                    ((role . "user")
                                     (content . ,freddy-ai-prompt))])))))
    (url-retrieve freddy-ai-base-url 'freddy-ai/handle-response)))

(setq freddy-ai-api-key (freddy-ai/key-auth-source))
