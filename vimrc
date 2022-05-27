" Attempt to determine the type of a file based on its name and possibly its
" contents. Use this to allow intelligent auto-indenting for each filetype,
" and for plugins that are filetype specific.
filetype indent plugin on
 
" Enable syntax highlighting
syntax on
 
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
set cmdheight=2
set number
set notimeout ttimeout ttimeoutlen=200
set pastetoggle=<F11>
set shiftwidth=2
set softtabstop=2
set expandtab
set autoread
set wildignore+=*/tmp/*,*/node_modules/*,*/bower_components/*,*.so,*.swp,*.zip
set title
set clipboard=unnamed
set foldenable
set foldmethod=syntax
set foldlevelstart=20
set cursorline
set relativenumber
set thesaurus+=~/thesaurus.txt
set encoding=UTF-8

if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif


map Y y$

call plug#begin('~/.local/share/nvim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'mattn/emmet-vim', { 'for': ['html','css'] }
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'vim-airline/vim-airline'
Plug 'yggdroot/indentline'
Plug 'pangloss/vim-javascript', { 'for': 'js' }
Plug 'mxw/vim-jsx', { 'for': 'jsx' }
Plug 'tpope/vim-sensible'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'w0rp/ale'
Plug 'navarasu/onedark.nvim'
Plug 'tpope/vim-repeat'
Plug 'plasticboy/vim-markdown', { 'for': 'md' }
Plug 'christoomey/vim-tmux-navigator'
Plug 'benmills/vimux'
Plug 'mhinz/vim-startify'
Plug 'ryanoasis/vim-devicons'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
call plug#end()

" On OSX
vmap <C-c> y:call system("pbcopy", getreg("\""))<CR>
nmap <C-v> :call setreg("\"",system("pbpaste"))<CR>p

let mapleader = " "
nmap <silent> <space>ww :wincmd w<CR>
nmap <silent> <space><tab> :bn<CR>
nmap <silent> <space>bd :bdelete<CR>
nmap <silent> <space>fs :w<CR>
"nmap <silent> <space>fr :ol<CR>

" Navigate tabs with C-l and C-h
nnoremap <C-l> :bnext<CR>
nnoremap <C-h> :bprev<CR>

" fugitive git mappings
nnoremap <space>gs :Git<CR> 
nnoremap <space>gp :Git push<CR>
nnoremap <space>gf :Git pull<CR>
nnoremap <space>gb :Git blame<CR>
nnoremap <space>gc :Git checkout<Space>
nnoremap <space>gu :Git push -u origin <Space>
nnoremap <space>gl :Git log<CR>

nnoremap <leader>el :lopen<CR>
nnoremap <leader>ec :lclose<CR>

nnoremap <leader>fed :tabe ~/.vimrc<CR>
nnoremap <leader>feR :source %<CR>
nnoremap <leader>bs :tabe scratch<CR>
nnoremap <space>fr :History<CR>


" Find files using Telescope command-line sugar.
nnoremap <leader>ff :Telescope find_files<cr>
nnoremap <leader>fg :Telescope live_grep<cr>
nnoremap <leader>bb :Telescope buffers<cr>
nnoremap <leader>fh :Telescope help_tags<cr>

map <space>pt :NERDTreeToggle<CR>

syntax enable
"set background=dark

colorscheme onedark 

let NERDTreeAutoDeleteBuffer = 1
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDTreeMapActivateNode = "l"
let NERDTreeMapCloseDir = "h"
let NERDTreeIgnore=['node_modules','bower_components','tmp']


"NERDTree File highlighting
"function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
"  exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='.  a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
"  exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'.  a:extension .'$#'
"endfunction

"call NERDTreeHighlightFile('hbs', 'green', 'none', 'green', '#151515')
"call NERDTreeHighlightFile('md', 'blue', 'none', '#3366FF', '#151515')
"call NERDTreeHighlightFile('yml', 'yellow', 'none', 'yellow', '#151515')
"call NERDTreeHighlightFile('config', 'yellow', 'none', 'yellow', '#151515')
"call NERDTreeHighlightFile('conf', 'yellow', 'none', 'yellow', '#151515')
"call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#151515')
"call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#151515')
"call NERDTreeHighlightFile('css', 'cyan', 'none', 'cyan', '#151515')
"call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#151515')



" Ale 
" Set this. Airline will handle the rest.
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'default'
let g:airline#extensions#tabline#tab_nr_type = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline_solarized_bg='dark'


" Ale config
let g:ale_fixers = {  'javascript': ['eslint', 'prettier']  }
let g:ale_fix_on_save = 1
let g:ale_linters = {'javascript': ['eslint']}


autocmd Filetype help nnoremap <CR> <C-]>
autocmd Filetype help nnoremap <BS> <C-T>

let g:mustache_abbreviations = 1
au BufRead,BufNewFile *.hbs setfiletype mustache

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor
endif

" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

let g:netrw_banner = 0
let g:netrw_liststyle = 3
"let g:netrw_browse_split = 3
let g:netrw_altv = 1
let g:netrw_winsize = 25
let g:netrw_list_hide = &wildignore
let g:netrw_preview = 1

nnoremap ]b :bn<cr>
nnoremap [b :bp<cr>

" Prompt for a command to run
map <Leader>vp :VimuxPromptCommand<CR>
" Run last command executed by VimuxRunCommand
map <Leader>vl :VimuxRunLastCommand<CR>

"Disabling conceal for code fences requires an additional setting:
let g:vim_markdown_conceal_code_blocks = 0
"Disabling conceal for all other things
let g:vim_markdown_conceal = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Splits and Tabbed Files
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set splitbelow splitright

" Remap splits navigation to just CTRL + hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Make adjusing split sizes a bit more friendly
noremap <silent> <C-Left> :vertical resize +3<CR>
noremap <silent> <C-Right> :vertical resize -3<CR>
noremap <silent> <C-Up> :resize +3<CR>
noremap <silent> <C-Down> :resize -3<CR>

" Change 2 split windows from vert to horiz or horiz to vert
map <Leader>th <C-w>t<C-w>H
map <Leader>tk <C-w>t<C-w>K

" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

