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


-- [[ Basic Keymaps ]]
-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

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
  }
  
})

-- [[ Setting options ]]
-- See `:help vim.o`

-- No swap file
vim.o.swapfile = false

-- set tabstop
vim.o.tabstop = 2
--set shiftwidth
vim.o.shiftwidth = 2

-- Expand tabs
vim.o.expandtab = true

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- Set colorscheme
vim.o.termguicolors = true

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- Set cursorline
vim.o.cursorline = true

-- Set relative line number
vim.o.relativenumber = true


-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- Set lualine as statusline
-- See `:help lualine.txt`
require('lualine').setup {
  options = {
    icons_enabled = false,
    theme = 'dracula',
    component_separators = '|',
    section_separators = '',
  },
}


-- Enable `lukas-reineke/indent-blankline.nvim`
-- See `:help indent_blankline.txt`
require('indent_blankline').setup {
  char = '┊',
  show_trailing_blankline_indent = false,
}

-- Gitsigns
-- See `:help gitsigns.txt`
require('gitsigns').setup {
  signs = {
    add = { text = '+' },
    change = { text = '~' },
    delete = { text = '_' },
    topdelete = { text = '‾' },
    changedelete = { text = '~' },
  },
}

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
  },
}

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
require('nvim-treesitter.configs').setup {
  -- Add languages to be installed here that you want installed for treesitter
  ensure_installed = {  'lua', 'javascript', 'html', 'css', 'json', 'typescript', 'vim', 'glimmer', 'tsx', 'svelte' },

  highlight = { enable = true },
  indent = { enable = true, disable = { 'python' } },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<c-space>',
      node_incremental = '<c-space>',
      scope_incremental = '<c-s>',
      node_decremental = '<c-backspace>',
    },
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ['aa'] = '@parameter.outer',
        ['ia'] = '@parameter.inner',
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
      },
      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },
      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ['<leader>a'] = '@parameter.inner',
      },
      swap_previous = {
        ['<leader>A'] = '@parameter.inner',
      },
    },
  },
}


-- LSP settings.
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--
--  Add any additional override configuration in the following tables. They will be passed to
--  the `settings` field of the server config. You must look up that documentation yourself.
local servers = {
  -- clangd = {},
  -- gopls = {},
  -- pyright = {},
  -- rust_analyzer = {},
  tsserver = {},
  html = {},
  cssls = {},
  eslint = {},
  ember = {},

  luau_lsp = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
}

-- Setup neovim lua configuration
require('neodev').setup()
--
-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Setup mason so it can manage external tooling
require('mason').setup()

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require('lspconfig')[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
    }
  end,
}

-- Turn on lsp status information
-- require('fidget').setup()
 
-- nvim-cmp setup
local cmp = require 'cmp'
local luasnip = require 'luasnip'

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
  },
}


-- load snippets from path/of/your/nvim/config/my-cool-snippets
require("luasnip.loaders.from_vscode").lazy_load()

-- disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1


-- setup glimmer for Ember templates
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.glimmer = {
  filetype = "hbs",
  used_by = {
    "handlebars",
    "html.handlebars"
  }
}

-- eslint setup
require('lspconfig').eslint.setup({
  on_attach = function(client, bufnr)
    print('eslint started')
  end,
})

local null_ls = require("null-ls")

local group = vim.api.nvim_create_augroup("lsp_format_on_save", { clear = false })
local event = "BufWritePre" -- or "BufWritePost"
local async = event == "BufWritePost"

null_ls.setup({
  on_attach = function(client, bufnr)
    if client.supports_method("textDocument/formatting") then
      vim.keymap.set("n", "<Leader>f", function()
        vim.lsp.buf.format({ bufnr = vim.api.nvim_get_current_buf() })
      end, { buffer = bufnr, desc = "[lsp] format" })

      -- format on save
      vim.api.nvim_clear_autocmds({ buffer = bufnr, group = group })
      vim.api.nvim_create_autocmd(event, {
        buffer = bufnr,
        group = group,
        callback = function()
          vim.lsp.buf.format({ bufnr = bufnr, async = async })
        end,
        desc = "[lsp] format on save",
      })
    end

    if client.supports_method("textDocument/rangeFormatting") then
      vim.keymap.set("x", "<Leader>f", function()
        vim.lsp.buf.format({ bufnr = vim.api.nvim_get_current_buf() })
      end, { buffer = bufnr, desc = "[lsp] format" })
    end
  end,
})

-- Prettier
local prettier = require("prettier")

prettier.setup({
  bin = 'prettier', -- or `'prettierd'` (v0.22+)
  filetypes = {
    "css",
    "html",
    "javascript",
    "javascriptreact",
    "json",
    "typescript",
    "typescriptreact",
    "yaml",
  },
  cli_options = {
    config_precedence = "prefer-file", -- or "cli-override" or "file-override"
  },
})


local map = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

local nmap = function(keys, func, desc)
  vim.keymap.set('n', keys, func, { noremap = true, silent = true, desc = desc })
end

local autocmd = function(keys, func, pattern)
  vim.api.nvim_create_autocmd('FileType', {
    command = "nmap <buffer> " .. keys .. " " .. "<Cmd>" .. func .. "<CR>",
    pattern = pattern,
  })
end


-- Edit config
nmap('<leader>.', ":tabe ~/.config/nvim/init.lua<CR>", 'Edit Neovim config')

-- Diagnostic keymaps
nmap('[d', vim.diagnostic.goto_prev, 'Go to previous diagnostics')
nmap(']d', vim.diagnostic.goto_next, 'Go to next diagnostics')
nmap('<leader>e', vim.diagnostic.open_float, 'Open floating diagnostics')
nmap('<leader>q', vim.diagnostic.setloclist, 'Set location list')

-- Fugitive key mappings
nmap('<Leader>gs', ':Git<CR>', 'Git') 
nmap('<Leader>gp', ':Git push<CR>', 'Git push')
nmap('<Leader>gf', ':Git pull<CR>', 'Git pull')
nmap('<Leader>gb', ':Git blame<CR>', 'Git blame')
nmap('<Leader>gc', ':Git checkout<Space>', 'Git checkout')
nmap('<Leader>gu', ':Git push -u origin <Space>', 'Git push origin')
nmap('<Leader>gl', ':Git log<CR>', 'Git log')

autocmd('q','q', { 'fugitiveblame', 'fugitive' })

-- Telescope key mappings
nmap('<leader>?', ':Telescope oldfiles<cr>' , '[?] Find recently opened files')
nmap('<leader><space>', ':Telescope buffers<cr>', '[ ] Find existing buffers')
nmap('<leader>/', function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
    winblend = 10,
    previewer = false,
  })
end, '[/] Fuzzily search in current buffer]')

nmap('<leader>sf', ':Telescope find_files<cr>', '[S]earch [F]iles')
nmap('<leader>sh', ':Telescope help_tags<cr>', '[S]earch [H]elp')
nmap('<leader>sw', ':Telescope grep_string<cr>', '[S]earch current [W]ord')
nmap('<leader>sg', ':Telescope live_grep<cr>', '[S]earch by [G]rep')
nmap('<leader>sd', ':Telescope diagnostics<cr>', '[S]earch [D]iagnostics')

-- Barbar key mappings
-- Move to previous/next
nmap('[b', '<Cmd>BufferPrevious<CR>', 'Go to previous buffer')
nmap(']b', '<Cmd>BufferNext<CR>', 'Go to next buffer')
-- Re-order to previous/next
nmap('<Space>b<', '<Cmd>BufferMovePrevious<CR>', 'Move previous buffer')
nmap('<Space>b>', '<Cmd>BufferMoveNext<CR>', 'Move next buffer')
-- Goto buffer in position...
nmap('<Space>b1', '<Cmd>BufferGoto 1<CR>', 'Go to buffer 1')
nmap('<Space>b2', '<Cmd>BufferGoto 2<CR>', 'Go to buffer 2')
nmap('<Space>b0', '<Cmd>BufferLast<CR>', 'Go to buffer 3')
-- Close buffer
nmap('<Space>bd', '<Cmd>BufferClose<CR>', 'Delete Buffer')
-- Pick buffer
nmap('gb', '<Cmd>BufferPick<CR>', 'Pick buffer')

-- vimux mappings
nmap('<Leader>v', 'Vimux')
nmap('<Leader>vr', '<Cmd>VimuxPromptCommand<CR>', 'Vimux Prompt Command')
nmap('<Leader>vl', '<Cmd>VimuxRunLastCommand<CR>', 'Vimux Run Last Command')

-- Octo key mappings
nmap('<Space>o','Octo')
nmap('<Space>op','Pull Requests')
nmap('<Space>opl','<Cmd>Octo pr list<CR>', 'List pull requests')

nmap('<Space>oi','Issues')
nmap('<Space>oil','<Cmd>Octo issue list<CR>', 'List issues')
nmap('<Space>oin','<Cmd>Octo issue create<CR>', 'New issue')
nmap('<Space>oib','<Cmd>Octo issue browser<CR>', 'Open current issue in the browser')
nmap('<Space>oiu','<Cmd>Octo issue url<CR>', 'Copies the URL of the current issue to the system clipboard')

autocmd('b','Octo issue browser', { 'octo' })
autocmd('u','Octo issue url', { 'octo' })
autocmd('q','BufferClose', { 'octo' })

autocmd('f','Git pull', { 'fugitive' })
autocmd('p','Git push', { 'fugitive' })

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
