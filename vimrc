source ~/.vim/settings.vim
source ~/.vim/mappings.vim
source ~/.vim/plugins.vim

let g:javascript_conceal_function             = "ƒ"
let g:javascript_conceal_null                 = "ø"
let g:javascript_conceal_this                 = "@"
let g:javascript_conceal_return               = "⇚"
let g:javascript_conceal_undefined            = "¿"
let g:javascript_conceal_NaN                  = "ℕ"
let g:javascript_conceal_prototype            = "¶"
let g:javascript_conceal_static               = "•"
let g:javascript_conceal_super                = "Ω"
let g:javascript_conceal_arrow_function       = "⇒"
let g:javascript_conceal_noarg_arrow_function = "🞅"
let g:javascript_conceal_underscore_arrow_function = "🞅"

colorscheme onedark

inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? asyncomplete#close_popup() : "\<cr>"


let g:NERDTreeIgnore = ['^node_modules$', '^coverage$']

" Enable tabline to show buffer list
let g:airline#extensions#tabline#enabled = 1

"keep your conceal setting
let g:indentLine_setConceal = 0

augroup load_snippets
  autocmd!
  autocmd InsertEnter * call plug#load('vim-vsnip', 'vim-vsnip-integ','friendly-snippets')
        \| autocmd! load_snippets
augroup END

let g:hardtime_default_on = 1
