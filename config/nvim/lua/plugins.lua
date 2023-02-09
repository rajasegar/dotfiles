return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use 'tpope/vim-fugitive'
  use 'airblade/vim-gitgutter'
  use { 'mattn/emmet-vim',  ft =  {'html','css'} }
  use { 'scrooloose/nerdtree',  on = 'NERDTreeToggle' }
  use 'vim-airline/vim-airline'
  use 'yggdroot/indentline'
  use { 'pangloss/vim-javascript',  ft = {'js'} }
  use { 'mxw/vim-jsx',  ft = {'jsx'} }
  use 'tpope/vim-sensible'
  use 'scrooloose/nerdcommenter'
  use 'tpope/vim-surround'
  use 'w0rp/ale'
  use 'navarasu/onedark.nvim'
  use 'tpope/vim-repeat'
  use { 'plasticboy/vim-markdown',  ft = {'md'} }
  use 'christoomey/vim-tmux-navigator'
  use 'benmills/vimux'
  use 'mhinz/vim-startify'
  use 'ryanoasis/vim-devicons'
  use 'nvim-lua/plenary.nvim'
  use 'nvim-telescope/telescope.nvim'
  use { 'evanleck/vim-svelte', branch = 'main'}
  use { 'neoclide/coc.nvim', branch = 'release'}
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
  use 'jamessan/vim-gnupg'
  use 'mustache/vim-mustache-handlebars'

end)
