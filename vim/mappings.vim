"Open nerdtree on startup
"augroup ProjectDrawer
  "autocmd!
  "autocmd VimEnter * :NERDTree
"augroup END

let mapleader = " "
nmap <silent> <leader>ww :wincmd w<CR>
nmap <silent> <leader>; :w<CR>
nmap <silent> <leader>pt :NERDTreeToggle<CR>

" Buffer manipulation
nnoremap <leader>bb :Buffers<CR>
nmap <silent> <leader><tab> :bn<CR>
nmap <silent> ]b :bnext<CR>
nmap <silent> [b :bprev<CR>
nmap <silent> <leader>bd :bdelete<CR>

" config manipulation
nnoremap <leader>. :tabe ~/.vimrc<CR>
nnoremap <leader>om :tabe ~/.vim/mappings.vim<CR>
nnoremap <leader>os :tabe ~/.vim/settings.vim<CR>
nnoremap <leader>op :tabe ~/.vim/plugins.vim<CR>
nnoremap <leader>bs :tabe scratch<CR>
nnoremap <space>fr :History<CR>

nnoremap <C-g> :exe '!tmux popup -d "\#{pane_current_path}" -xC -yC -w80\% -h80\% -E lazygit'<CR>
nnoremap <leader>ff :Files<CR>

" Tab for autocompletion
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? asyncomplete#close_popup() : "\<cr>"


