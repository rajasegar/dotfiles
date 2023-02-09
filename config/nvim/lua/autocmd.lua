vim.cmd([[
autocmd FileType fugitiveblame nmap <buffer> q gq
autocmd FileType fugitive nmap <buffer> q gq

autocmd Filetype help nnoremap <CR> <C-]>
autocmd Filetype help nnoremap <BS> <C-T>
autocmd FileType help noremap <buffer> q :q<cr>
]])
