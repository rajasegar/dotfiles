(prodigy-define-service
  :name "Root config"
  :command "npm"
  :cwd "~/www/multi-ember-mfe/root-config"
  :args '("start")
  :port 9000
  :tags '(root-config node)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Navbar MFE"
  :command "npm"
  :cwd "~/www/multi-ember-mfe/navbar"
  :args '("start")
  :port 8080
  :tags '(mfe node)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "People MFE"
  :command "npm"
  :cwd "~/www/multi-ember-mfe/people"
  :args '("run" "dev")
  :port 4200
  :tags '(mfe ember)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Planets MFE"
  :command "npm"
  :cwd "~/www/multi-ember-mfe/planets"
  :args '("run" "dev")
  :port 4201
  :tags '(mfe ember)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - Gemma 3 270m"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "gemma-3-270m-it-qat-Q4_0.gguf" "--port" "8000")
  :port 8000
  :tags '(llama-cpp gemma)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)


(prodigy-define-service
  :name "LLM - Qwen 2.5 Coder"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "qwen2.5-coder-1.5b-instruct-q2_k.gguf" "--port" "8000")
  :port 8000
  :tags '(llama-cpp qwen)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - Llama 3.2"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "Llama-3.2-1B-Instruct-IQ4_NL.gguf" "--port" "8000")
  :port 8000
  :tags '(llama-cpp llama)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - Nemotron"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "NVIDIA-Nemotron3-Nano-4B-Q4_K_M.gguf" "--port" "8000")
  :port 8000
  :tags '(llama-cpp nemotron)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - Qwen 3.5"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "Qwen3.5-0.8B-BF16.gguf" "--port" "8000")
  :port 8000
  :tags '(llama-cpp qwen)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - SmolLM2-1"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "SmolLM2-1.7B-Instruct-Q2_K.gguf" "--port" "8000")
  :port 8000
  :tags '(llama-cpp smollm)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)
