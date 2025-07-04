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
        'justinmk/vim-dirvish'
    },

    {
        'romainl/vim-malotru'
    },

    {
        'pbrisbin/vim-colors-off'
    },

    {
        'tpope/vim-surround'
    },

    {
        'Lokaltog/vim-monotone',
        priority = 1000, -- load first
        config = function ()
            vim.cmd([[
                augroup custom_appearance
                  autocmd!
                  au ColorScheme * hi Normal gui=NONE guifg=NONE guibg=NONE ctermfg=none ctermbg=NONE
		  " au ColorScheme * hi Statusline guifg=#000000 guibg=#e5e5e5 gui=bold
                augroup END    
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
                  colorscheme monotone
            ]])
        end,
    },
    {
        'yankcrime/nvim-grey'
    },
    {
	'greggh/claude-code.nvim',
	requires = {
		'nvim-lua/plenary.nvim', -- Required for git operations
	},
	config = function()
		require('claude-code').setup()
	end
    },
    {
        'yasukotelin/shirotelin',
    },
    {
	'robertmeta/nofrils'
    },
    {
      'nvim-telescope/telescope-fzf-native.nvim',
      build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build'
    },
    
    {'nvim-telescope/telescope-ui-select.nvim' },

    {'towolf/vim-helm'},

    {
      "nvim-telescope/telescope.nvim", 
      tag = '0.1.8',
      dependencies = { 
        "nvim-lua/plenary.nvim" ,
        "nvim-treesitter/nvim-treesitter",
        "nvim-tree/nvim-web-devicons",
      },
      config = function ()
	local actions = require("telescope.actions")
        require("telescope").setup({
          extensions = {
            fzf = {
              fuzzy = true,
              override_generic_sorter = true,
              override_file_sorter = true,
              case_mode = "smart_case",
            }
          },
	  defaults = {
            prompt_prefix = '',
            entry_prefix = ' ',
            selection_caret = ' ',
            layout_config = {
              prompt_position = 'bottom',
              width = 0.7,
              height = 0.7,
              preview_width = 0.6,
            },
	    mappings = {
              i = {
                  ["<esc>"] = actions.close,
		  ["<C-j>"] = actions.move_selection_next,
                  ["<C-k>"] = actions.move_selection_previous
              },
	    },
	  },
        })
        require("telescope").load_extension("fzf")
        require("telescope").load_extension("ui-select")
      end,
    },

    {
    "astephanh/yaml-companion.nvim",
      config = function()
	branch = "kubernetes_crd_detection",
        require("telescope").load_extension("yaml_schema")
        require("yaml-companion").setup({
  	  builtin_matchers = {
  	    kubernetes = { enabled = true },
  	    cloud_init = { enabled = true }
  	  },
 	  schemas = {
     	    name = "Kubernetes 1.30",
	    uri = "https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.30.1-standalone-strict/all.json",
  	  },
        })
      end
    },

  -- Language server (LSP)
  {
    "neovim/nvim-lspconfig", 
    dependencies = { 'saghen/blink.cmp' },
    config = function ()
      util = require "lspconfig/util"

      local capabilities = require('blink.cmp').get_lsp_capabilities()
      capabilities.textDocument.completion.completionItem.snippetSupport = true
      local lspconfig = require('lspconfig')

      -- Golang
      require("lspconfig").gopls.setup({
        capabilities = capabilities,
        flags = { debounce_text_changes = 200 },
        settings = {
          gopls = {
            usePlaceholders = true,
            gofumpt = true,
            analyses = {
              nilness = true,
              unusedparams = true,
              unusedwrite = true,
              useany = true,
            },
            codelenses = {
              gc_details = false,
              generate = true,
              regenerate_cgo = true,
              run_govulncheck = true,
              test = true,
              tidy = true,
              upgrade_dependency = true,
              vendor = true,
            },
            experimentalPostfixCompletions = true,
            completeUnimported = true,
            staticcheck = true,
            directoryFilters = { "-.git", "-node_modules" },
            semanticTokens = true,
            hints = {
              assignVariableTypes = true,
              compositeLiteralFields = true,
              compositeLiteralTypes = true,
              constantValues = true,
              functionTypeParameters = true,
              parameterNames = true,
              rangeVariableTypes = true,
            },
          },
        },
      })

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities.textDocument.completion.completionItem.snippetSupport = true

      -- Terraform / HCL
      require("lspconfig").terraformls.setup({
        on_attach = on_attach,
        flags = { debounce_text_changes = 150 },
        capabilities = capabilities,	
      })
      vim.api.nvim_create_autocmd({"BufWritePre"}, {
        pattern = {"*.tf", "*.tfvars"},
        callback = function()
          vim.lsp.buf.format()
        end,
      })

      --- Helm
      require("lspconfig").helm_ls.setup{
        settings = {
          ['helm-ls'] = {
            yamlls = {
              path = "yaml-language-server",
            }
          }
	}
      }

      -- Ansible
      require("lspconfig").ansiblels.setup{}
      require("lspconfig").yamlls.setup{}
    end,
  },

  -- Autocompletion
  {
    'saghen/blink.cmp',
    -- optional: provides snippets for the snippet source
    dependencies = { 'rafamadriz/friendly-snippets' },
  
    version = '1.*',
    opts = {
      -- 'default' (recommended) for mappings similar to built-in completions (C-y to accept)
      -- 'super-tab' for mappings similar to vscode (tab to accept)
      -- 'enter' for enter to accept
      -- 'none' for no mappings
      --
      -- All presets have the following mappings:
      -- C-space: Open menu or open docs if already open
      -- C-n/C-p or Up/Down: Select next/previous item
      -- C-e: Hide menu
      -- C-k: Toggle signature help (if signature.enabled = true)
      --
      -- See :h blink-cmp-config-keymap for defining your own keymap
      keymap = { preset = 'super-tab' },
  
      appearance = {
        -- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
        -- Adjusts spacing to ensure icons are aligned
        nerd_font_variant = 'mono'
      },
  
      -- (Default) Only show the documentation popup when manually triggered
      completion = { documentation = { auto_show = false } },
  
      -- Default list of enabled providers defined so that you can extend it
      -- elsewhere in your config, without redefining it, due to `opts_extend`
      sources = {
        default = { 'lsp', 'path', 'snippets', 'buffer' },
      },
  
      fuzzy = { implementation = "prefer_rust_with_warning" }
    },
    opts_extend = { "sources.default" }
  },


  --- Highlight, edit, and navigate code
  {
    "windwp/nvim-autopairs",
    config = function() 
      require("nvim-autopairs").setup {
        check_ts = true,
      }
    end
  },
  { 
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
    },
    build = ":TSUpdate",
    config = function()
      require('nvim-treesitter.configs').setup({
        ensure_installed = {
          'go',
          'gomod',
          'proto',
          'lua',
          'ruby',
          'vimdoc',
          'vim',
          'bash',
          'fish',
	  'hcl',
	  'python',
          'json',
          'markdown',
          'markdown_inline',
          'mermaid',
	  'yaml'
        },
        indent = { enable = true },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<space>", -- maps in normal mode to init the node/scope selection with space
            node_incremental = "<space>", -- increment to the upper named parent
            node_decremental = "<bs>", -- decrement to the previous node
            scope_incremental = "<tab>", -- increment to the upper scope (as defined in locals.scm)
          },
        },
        autopairs = {
          enable = true,
        },
        highlight = {
          enable = true,

          -- Disable slow treesitter highlight for large files
          disable = function(lang, buf)
            local max_filesize = 100 * 1024 -- 100 KB
            local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
            if ok and stats and stats.size > max_filesize then
              return true
            end
          end,

          -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
          -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
          -- Using this option may slow down your editor, and you may see some duplicate highlights.
          -- Instead of true it can also be a list of languages
          additional_vim_regex_highlighting = false,
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
              ["iB"] = "@block.inner",
              ["aB"] = "@block.outer",
            },
          },
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              [']]'] = '@function.outer',
            },
            goto_next_end = {
              [']['] = '@function.outer',
            },
            goto_previous_start = {
              ['[['] = '@function.outer',
            },
            goto_previous_end = {
              ['[]'] = '@function.outer',
            },
          },
          swap = {
            enable = true,
            swap_next = {
              ['<leader>sn'] = '@parameter.inner',
            },
            swap_previous = {
              ['<leader>sp'] = '@parameter.inner',
            },
          },
        },
      })
    end,
  },


})

--- Filetypes

vim.filetype.add({
  extension = {
    yml = 'yaml.ansible',
    hcl = 'terraform'
  }
})

vim.api.nvim_create_augroup("YamlHelmSettings", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
  group = "YamlHelmSettings",
  pattern = { "yaml", "helm" },
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.expandtab = true
  end,
})

--- Keymappings

vim.g.mapleader = ' ' -- Space

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<C-p>', builtin.git_files, {})
vim.keymap.set('n', '<C-f>', builtin.find_files, {})
vim.keymap.set('n', '<C-s>', builtin.live_grep, {})
vim.keymap.set('n', '<C-b>', builtin.buffers, {})
vim.keymap.set('n', '<C-g>', builtin.lsp_document_symbols, {})
vim.keymap.set('n', '<C-y>', ':Telescope yaml_schema<CR>', {})
vim.keymap.set('n', '<leader>td', builtin.diagnostics, {})
vim.keymap.set('n', '<leader>gs', builtin.grep_string, {})
vim.keymap.set('n', '<leader>gg', builtin.live_grep, {})

vim.keymap.set('n', '<Leader><space>', ':nohlsearch<CR>')
vim.keymap.set('n', '<Leader>tn', ':tabnext<CR>')
vim.keymap.set('n', '<Leader>tp', ':tabprevious<CR>')

--- Standard (neo)vim options

--- vim.opt.termguicolors = true
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
vim.opt.signcolumn = "no"
vim.opt.statuscolumn = "%=%s%C%l "
vim.opt.laststatus=3

