" Attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" and for plugins that are filetype specific.
filetype indent plugin on
 
" Enable syntax highlighting
" syntax on
 
 
set nocompatible
set hidden
set noswapfile
set path+=**
set wildmenu
set showcmd
set hlsearch
set ignorecase
set smartcase
set backspace=indent,eol,start
set autoindent
set nostartofline
set ruler
set laststatus=2
set confirm
set visualbell
set t_vb=
set mouse=a
set cmdheight=1
set number
set notimeout ttimeout ttimeoutlen=0
set pastetoggle=<F11>
set shiftwidth=2
set softtabstop=2
set expandtab
set autoread
set rtp+=~/.fzf
set rtp+=~/.vim
set wildignore+=*/tmp/*,*/bower_components/*,*.so,*.swp,*.zip
set wildignore+=*/node_modules/*
set title
set clipboard+=unnamedplus
set foldenable
set foldmethod=syntax
set foldlevelstart=20
set cursorline
set relativenumber
set thesaurus+=~/thesaurus.txt
set encoding=utf-8


