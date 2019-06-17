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
" let Vundle manage Vundle, required
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.fzf
set wildignore+=*/tmp/*,*/node_modules/*,*/bower_components/*,*.so,*.swp,*.zip
set title
set clipboard=unnamed
set foldenable
set foldmethod=syntax
set foldlevelstart=20
set cursorline
set relativenumber

map Y y$
 
" Map <C-L> (redraw screen) to also turn off search highlighting until the
" next search
nnoremap <C-L> :nohl<CR><C-L>
 
"------------------------------------------------------------
" To automatically refresh file changes

call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'mattn/emmet-vim'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-airline/vim-airline'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'tpope/vim-sensible'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-surround'
Plugin 'w0rp/ale'
Plugin 'valloric/youcompleteme'
Plugin 'ternjs/tern_for_vim'
Plugin 'honza/vim-snippets'
Plugin 'SirVer/ultisnips'
Plugin 'junegunn/fzf.vim'
Plugin 'tpope/vim-repeat'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'tpope/vim-rhubarb'
Plugin 'AndrewRadev/ember_tools.vim'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'JamshedVesuna/vim-markdown-preview'
Plugin 'heavenshell/vim-jsdoc'
Plugin 'beloglazov/vim-online-thesaurus'
Plugin 'digitaltoad/vim-pug'
Plugin 'rajasegar/vim-search-web'
call vundle#end()

autocmd vimenter * NERDTree


" On OSX
vmap <C-c> y:call system("pbcopy", getreg("\""))<CR>
nmap <C-v> :call setreg("\"",system("pbpaste"))<CR>p

let mapleader = " "
nmap <silent> <space>ww :wincmd w<CR>
nmap <silent> <space><tab> :bn<CR>
nmap <silent> <space>bd :tabclose<CR>
nmap <silent> <space>fs :w<CR>

" Navigate tabs with C-l and C-h
nnoremap <C-l> gt
nnoremap <C-h> gT

" fugitive git mappings
nnoremap <space>gs :Gstatus<CR> 
nnoremap <space>gp :Gpush<CR>
nnoremap <space>gf :Gpull<CR>
nnoremap <space>gb :Git branch<Space>
nnoremap <space>gc :Git checkout<Space>
nnoremap <space>gl :Git log<CR>


nnoremap <leader>el :lopen<CR>
nnoremap <leader>ec :lclose<CR>

nnoremap <leader>fed :tabe ~/.vimrc<CR>
nnoremap <leader>feR :source %<CR>

" FZF mappings
nnoremap <silent> <leader>/ :Ag!<CR>
nnoremap <space>ff :FZF -m<CR>
nnoremap <space>pf :GFiles<CR>


"map <space>pt :NERDTreeToggle<CR>
map <space>pt :NERDTreeFind<CR>

syntax enable
set background=dark


let NERDTreeAutoDeleteBuffer = 1
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let NERDTreeMapActivateNode = "l"
let NERDTreeMapCloseDir = "h"
let NERDTreeIgnore=['node_modules','bower_components','tmp']

"NERDTree File highlighting
function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
  exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='.  a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
  exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'.  a:extension .'$#'
endfunction

call NERDTreeHighlightFile('hbs', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('md', 'blue', 'none', '#3366FF', '#151515')
call NERDTreeHighlightFile('yml', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('config', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('conf', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('css', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#151515')



" Ale 
" Set this. Airline will handle the rest.
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'default'
let g:airline#extensions#tabline#tab_nr_type = 1
let g:airline_solarized_bg='dark'

let g:indent_guides_guide_size = 1
let g:indent_guides_color_change_percent = 100
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_exclude_filetypes = ['help', 'nerdtree']
let g:indent_guides_auto_colors = 0



" Ale config
let g:ale_fixers = {  'javascript': ['eslint', 'prettier']  }
let g:ale_fix_on_save = 1
let g:ale_linters = {'javascript': ['eslint']}


" Trigger configuration. Do not use <tab> if you use
" https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger = "<nop>" 
inoremap <expr> <CR> pumvisible() ? "<C-R>=UltiSnips#ExpandSnippetOrJump()<CR>" : "\<CR>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"



command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview( {'options': '--literal --delimiter : --nth 4..'},'up:60%')
  \                         : fzf#vim#with_preview( 'right:50%:hidden', '?'),
  \                 <bang>0)

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

let vim_markdown_preview_toggle=2
let vim_markdown_preview_browser='Google Chrome'

let g:netrw_banner = 0
let g:netrw_liststyle = 3
"let g:netrw_browse_split = 3
let g:netrw_altv = 1
let g:netrw_winsize = 25
let g:netrw_list_hide = &wildignore
let g:netrw_preview = 1

let g:tern_map_keys = 1


nnoremap gb :bn<cr>
