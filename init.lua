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

--- Plugins

require("lazy").setup({
    {
        'Lokaltog/vim-monotone',
        priority = 1000, -- load first
        config = function ()
            vim.cmd([[
                augroup custom_appearance
                  autocmd!
                  au ColorScheme * hi Normal gui=NONE guifg=NONE guibg=NONE ctermfg=none ctermbg=none
                augroup END    
                colorscheme monotone
                function! s:statusline_expr()
                    let mod = "%{&modified ? '[+] ' : !&modifiable ? '[x] ' : ''}"
                    let ro  = "%{&readonly ? '[RO] ' : ''}"
                    let ft  = "%{len(&filetype) ? '['.&filetype.'] ' : ''}"
                    let fug = "%{exists('g:loaded_fugitive') ? fugitive#statusline() : ''}"
                    let sep = ' %= '
                    let pos = ' %-5(%l:%c%V%) '
                    let pct = ' %P '
                  
                    return ' [%n] %.40F %<'.mod.ro.ft.fug.sep.pos.'%*'.pct
                  endfunction
                  let &statusline = s:statusline_expr()
            ]])
        end,
    },

    {
        'justinmk/vim-dirvish'
    },

    {
      'nvim-telescope/telescope-fzf-native.nvim',
      build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build'
    },
    
    {'nvim-telescope/telescope-ui-select.nvim' },

    {
      "nvim-telescope/telescope.nvim", 
      tag = '0.1.4',
      dependencies = { 
        "nvim-lua/plenary.nvim" ,
        "nvim-treesitter/nvim-treesitter",
        "nvim-tree/nvim-web-devicons",
      },
      config = function ()
        require("telescope").setup({
          extensions = {
            fzf = {
              fuzzy = true,
              override_generic_sorter = true,
              override_file_sorter = true,
              case_mode = "smart_case",
            }
          }
        })
  
        require('telescope').load_extension('fzf')
        require("telescope").load_extension("ui-select")
      end,
    },
})

--- Keymappings

vim.g.mapleader = ' ' -- Space

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<C-p>', builtin.git_files, {})
vim.keymap.set('n', '<C-f>', builtin.find_files, {})
vim.keymap.set('n', '<C-b>', builtin.buffers, {})
vim.keymap.set('n', '<C-g>', builtin.lsp_document_symbols, {})
vim.keymap.set('n', '<leader>td', builtin.diagnostics, {})
vim.keymap.set('n', '<leader>gs', builtin.grep_string, {})
vim.keymap.set('n', '<leader>gg', builtin.live_grep, {})

vim.keymap.set('n', '<Leader><space>', ':nohlsearch<CR>')

--- Standard (neo)vim options

vim.opt.termguicolors = true
vim.opt.number = true
vim.opt.showmatch = true
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.swapfile = false
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.mouse = ""
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.modelines = 5

