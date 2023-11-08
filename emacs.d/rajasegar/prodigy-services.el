(prodigy-define-service
  :name "Root config"
  :command "npm"
  :cwd "~/www/multi-ember-mfe/root-config"
  :args '("start")
  :port 9000
  :tags '(root-config node))

(prodigy-define-service
  :name "Navbar MFE"
  :command "npm"
  :cwd "~/www/multi-ember-mfe/navbar"
  :args '("start")
  :port 8080
  :tags '(mfe node))

(prodigy-define-service
  :name "People MFE"
  :command "npm"
  :cwd "~/www/multi-ember-mfe/people"
  :args '("run" "dev")
  :port 4200
  :tags '(mfe ember))

(prodigy-define-service
  :name "Planets MFE"
  :command "npm"
  :cwd "~/www/multi-ember-mfe/planets"
  :args '("run" "dev")
  :port 4201
  :tags '(mfe ember))
