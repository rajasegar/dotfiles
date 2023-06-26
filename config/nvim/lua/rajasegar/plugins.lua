-- Install lazy
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)



require('lazy').setup({

  { -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',

      -- Additional lua configuration, makes nvim stuff amazing
      'folke/neodev.nvim',
    },
  },

  { -- Autocompletion
    'hrsh7th/nvim-cmp',
    -- load cmp on InsertEnter
    event = "InsertEnter",
    dependencies = { 'hrsh7th/cmp-nvim-lsp', 
      {
        'L3MON4D3/LuaSnip',
        dependencies = { "rafamadriz/friendly-snippets" }
      }, 
      'saadparwaiz1/cmp_luasnip' },
  },

  -- Highlight, edit, and navigate code
  { 
    'nvim-treesitter/nvim-treesitter',
    config = function()
      pcall(require('nvim-treesitter.install').update { with_sync = true })
    end,
  },

  { -- Additional text objects via treesitter
    'nvim-treesitter/nvim-treesitter-textobjects',
  },

  -- barbar
  'romgrk/barbar.nvim',

  -- Git related plugins
  {
  'tpope/vim-fugitive',
    cmd = "Git"
  },
  'lewis6991/gitsigns.nvim',

  {
    "Mofiqul/dracula.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      --load the colorscheme here
      vim.cmd[[colorscheme dracula]]
    end,
  },
  'nvim-lualine/lualine.nvim', -- Fancier statusline
  'lukas-reineke/indent-blankline.nvim', -- Add indentation guides even on blank lines
  {
  'numToStr/Comment.nvim', -- "gc" to comment visual regions/lines
    event = "BufReadPost",
    config = function()
      -- Enable Comment.nvim
      require('Comment').setup()
    end,
  },
  'tpope/vim-sleuth', -- Detect tabstop and shiftwidth automatically

  -- Fuzzy Finder (files, lsp, etc)
  { 'nvim-telescope/telescope.nvim', 
    lazy = true,
    branch = '0.1.x', 
    dependencies = { 'nvim-lua/plenary.nvim' } 
  },

  -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
  { 'nvim-telescope/telescope-fzf-native.nvim', build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build' },

  { "nvim-tree/nvim-web-devicons", lazy = true },

  -- Nvim tree
  {
    'nvim-tree/nvim-tree.lua',
    keys = {
      { "<leader>pt", "<cmd>NvimTreeToggle<cr>", desc = "NvimTree" },
    },
    config = function()
      require("nvim-tree").setup()
    end,
    dependencies = {
      'nvim-tree/nvim-web-devicons', -- optional, for file icons
    },
    tag = 'nightly' -- optional, updated every week. (see issue #1193)
  },

  -- Ember plugins
  {
    'mustache/vim-mustache-handlebars',
     ft = "hbs",
  },

  -- Prettier
  'jose-elias-alvarez/null-ls.nvim',
  'MunifTanjim/prettier.nvim',

  -- Smart yank
  'ibhagwan/smartyank.nvim',

  -- Octo
  {
    'pwntester/octo.nvim',
    cmd = "Octo",
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
      'nvim-tree/nvim-web-devicons',
    },
    config = function ()
      require"octo".setup()
    end
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    opts = { },
  },

  {
    "preservim/vimux",
    cmd = "VimuxPromptCommand"
  },

  -- nvim-surround
  {
    "kylechui/nvim-surround",
    version = "*",
    event = "VeryLazy",
    config = function()
        require("nvim-surround").setup({
        })
    end
  },

  -- symbols outline
  {
    'simrat39/symbols-outline.nvim',
    event = "VeryLazy"
  },

  -- colorizer
  {
    'norcalli/nvim-colorizer.lua',
    event = "VeryLazy"
  },

  -- rainbow parens
  {
    'HiPhish/nvim-ts-rainbow2',
    event = "VeryLazy"
  },

  -- rest.nvim
  {
    'rest-nvim/rest.nvim',
    event = "VeryLazy"
  }
})


