vim.cmd([[
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

call NERDTreeHighlightFile('hbs', 'green', 'none', 'green', '#282c34')
call NERDTreeHighlightFile('md', 'blue', 'none', '#3366FF', '#282c34')
call NERDTreeHighlightFile('yml', 'yellow', 'none', 'yellow', '#282c34')
call NERDTreeHighlightFile('config', 'yellow', 'none', 'yellow', '#282c34')
call NERDTreeHighlightFile('conf', 'yellow', 'none', 'yellow', '#282c34')
call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#282c34')
call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#282c34')
call NERDTreeHighlightFile('css', 'cyan', 'none', 'cyan', '#282c34')
call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#282c34')

]])
