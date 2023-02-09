vim.cmd([[
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

]])
