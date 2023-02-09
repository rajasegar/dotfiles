vim.cmd([[
" On OSX
vmap <C-c> y:call system("pbcopy", getreg("\""))<CR>
nmap <C-v> :call setreg("\"",system("pbpaste"))<CR>p

let mapleader = " "
nmap <silent> <space>ww :wincmd w<CR>
nmap <silent> <space><tab> :bn<CR>
nmap <silent> <space>bd :bdelete<CR>
nmap <silent> <space>fs :w<CR>

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

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Splits and Tabbed Files
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set splitbelow splitright

" Remap splits navigation to just CTRL + hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Change 2 split windows from vert to horiz or horiz to vert
map <Leader>th <C-w>t<C-w>H
map <Leader>tk <C-w>t<C-w>K



]])
