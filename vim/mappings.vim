"Open nerdtree on startup
"augroup ProjectDrawer
  "autocmd!
  "autocmd VimEnter * :NERDTree
"augroup END

let mapleader = " "
nmap <silent> <leader>ww :wincmd w<CR>
nmap <silent> <leader>; :w<CR>
nmap <silent> <leader>pt :NERDTreeFind<CR>

nnoremap <leader>/ :Ag<CR>
" Buffer manipulation
nnoremap <leader>bb :Buffers<CR>
nmap <silent> <leader><tab> :bn<CR>
nmap <silent> <leader>bd :bdelete<CR>

" config manipulation
nnoremap <leader>. :e ~/.vimrc<CR>
nnoremap <leader>om :e ~/.vim/mappings.vim<CR>
nnoremap <leader>os :e ~/.vim/settings.vim<CR>
nnoremap <leader>op :e ~/.vim/plugins.vim<CR>
nnoremap <leader>bs :e scratch<CR>
nnoremap <space>fr :History<CR>

nnoremap <C-g> :exe '!tmux popup -d "\#{pane_current_path}" -xC -yC -w80\% -h80\% -E lazygit'<CR>
nnoremap <leader>pf :GFiles<CR>
nnoremap <leader>ff :Files<CR>

" Tab for autocompletion
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? asyncomplete#close_popup() : "\<cr>"

" Line manipulation
nnoremap <C-j> :exe "normal ddp"<CR>
nnoremap <C-k> :exe "normal ddP"<CR>

" LSP key mappings
nmap gd <plug>(lsp-definition)
nmap gs <plug>(lsp-document-symbol-search)
nmap gS <plug>(lsp-workspace-symbol-search)
nmap gr <plug>(lsp-references)
nmap gi <plug>(lsp-implementation)
nmap gt <plug>(lsp-type-definition)
nmap <leader>rn <plug>(lsp-rename)
nmap [g <plug>(lsp-previous-diagnostic)
nmap ]g <plug>(lsp-next-diagnostic)
nmap K <plug>(lsp-hover)

" Vimux
nnoremap <leader>vr :VimuxPromptCommand<CR>
nnoremap <leader>vl :VimuxRunLastCommand<CR>
