call plug#begin()
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim', { 'on': ['Files','GFiles','Buffers'] }
Plug 'pangloss/vim-javascript', { 'for': 'js' }
Plug 'preservim/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'tpope/vim-sensible'
Plug 'vim-airline/vim-airline'
Plug 'yggdroot/indentline'
Plug 'joshdick/onedark.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'airblade/vim-gitgutter'
Plug 'preservim/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'evanleck/vim-svelte', {'branch': 'main', 'for': 'svelte'}
Plug 'hrsh7th/vim-vsnip', { 'on': [] }
Plug 'hrsh7th/vim-vsnip-integ', { 'on': [] }
Plug 'rafamadriz/friendly-snippets', { 'on': [] }
call plug#end()


