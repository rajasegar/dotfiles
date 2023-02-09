require('options')
require('plugins')
require('keymaps')
require('autocmd')
require('nerdtree')
require('linting')

vim.cmd([[

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
" Disable quote concealing in JSON files
let g:vim_json_conceal=0

" CoC extensions
let g:coc_global_extensions = ['coc-tsserver', 'coc-html', 'coc-css']

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ CheckBackspace() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
]])
