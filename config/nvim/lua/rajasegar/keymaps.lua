local nmap = function(keys, func, desc)
  vim.keymap.set('n', keys, func, { noremap = true, silent = true, desc = desc })
end

local autocmd = function(keys, func, pattern)
  vim.api.nvim_create_autocmd('FileType', {
    command = "nmap <buffer> " .. keys .. " " .. "<Cmd>" .. func .. "<CR>",
    pattern = pattern,
  })
end


-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

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

-- rest.nvim mappings
nmap('<Leader>r', 'REST')
nmap('<Leader>rr', '<Plug>RestNvim','Run the request under the cursor')
nmap('<Leader>rp', '<Plug>RestNvimPreview','Preview the request curl command')
nmap('<Leader>rl', '<Plug>RestNvimLast','Re-run the last request')
autocmd('q','q', { 'httpResult' })

-- Octo key mappings
nmap('<Space>o','Octo')
nmap('<Space>op','Pull Requests')
nmap('<Space>opl','<Cmd>Octo pr list<CR>', 'List pull requests')

nmap('<Space>oi','Issues')
nmap('<Space>oil','<Cmd>Octo issue list<CR>', 'List issues')
nmap('<Space>oin','<Cmd>Octo issue create<CR>', 'New issue')
nmap('<Space>oib','<Cmd>Octo issue browser<CR>', 'Open current issue in the browser')
nmap('<Space>oiu','<Cmd>Octo issue url<CR>', 'Copies the URL of the current issue to the system clipboard')

nmap('<Space>or','Review')
nmap('<Space>ors','<Cmd>Octo review start<CR>', 'Review Start')
nmap('<Space>oru','<Cmd>Octo review submit<CR>', 'Review S[u]bmit')
nmap('<Space>ord','<Cmd>Octo review discard<CR>', 'Review Discard')
nmap('<Space>orx','<Cmd>Octo review close<CR>', 'Review Exit')

autocmd('b','Octo issue browser', { 'octo' })
autocmd('u','Octo issue url', { 'octo' })
autocmd('q','BufferClose', { 'octo' })
autocmd('r','Octo review start', { 'octo' })

autocmd('f','Git pull', { 'fugitive' })
autocmd('p','Git push', { 'fugitive' })


nmap('<Leader>fs', ':w<CR>', 'Save File') 
