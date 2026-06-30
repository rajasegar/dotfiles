(prodigy-define-service
  :name "Ngrok Server"
  :command "npm"
  :cwd "/media/hdd/home/boot/Public/www/woocommerce"
  :args '("start")
  :port 3000
  :tags '(ngrok node)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Kids Sweat Shirt"
  :command "node"
  :cwd "/media/hdd/home/boot/Public/www/woocommerce"
  :args '("wordpress/kids-sweat-shirt.js")
  :tags '(totefy kids-sweat-shirt)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - Gemma 3"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "gemma-3-1b-it-Q4_K_M.gguf" "--port" "11434")
  :tags '(llama-cpp gemma)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - Gemma 3-270m"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "gemma-3-270m-it-F16.gguf" "--port" "11434")
  :tags '(llama-cpp gemma)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)


(prodigy-define-service
  :name "LLM - Qwen 2.5 Coder"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "qwen2.5-coder-1.5b-instruct-q2_k.gguf" "--port" "11434")
  :tags '(llama-cpp qwen)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - Llama 3.2"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "Llama-3.2-1B-Instruct-IQ4_NL.gguf" "--port" "11434")
  :tags '(llama-cpp llama)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - Nemotron"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "NVIDIA-Nemotron3-Nano-4B-Q4_K_M.gguf" "--port" "11434")
  :tags '(llama-cpp nemotron)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - Qwen 3.5"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "Qwen3.5-0.8B-Q4_0.gguf" "--port" "11434")
  :tags '(llama-cpp qwen)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "LLM - SmolLM2-1"
  :command "llama-server"
  :cwd "~/LLMs"
  :args '("-m" "SmolLM2-1.7B-Instruct-Q2_K.gguf" "--port" "11434")
  :tags '(llama-cpp smollm)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Dividend Calendar"
  :command "npm"
  :cwd "/media/hdd/home/boot/Public/www/dividend-calendar"
  :args '("run" "dev")
  :port 5173
  :tags '(vite react)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Stock Screener"
  :command "npm"
  :cwd "/media/hdd/home/boot/Public/www/stock-screener"
  :args '("run" "dev")
  :port 5174
  :tags '(vite react)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

