" .vimrc
" nick@dischord.org

" vim-plug plugin manager
call plug#begin('~/.vim/plugged')

Plug 'bling/vim-airline'
Plug 'kien/ctrlp.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'rodjek/vim-puppet'
Plug 'rking/ag.vim'
Plug 'cespare/vim-sbd'
Plug 'godlygeek/tabular'
Plug 'SirVer/ultisnips'
Plug 'airblade/vim-gitgutter'
Plug 'honza/vim-snippets'
Plug 'christoomey/vim-tmux-navigator'
Plug 'stephpy/vim-yaml'
Plug 'edkolev/tmuxline.vim'
Plug 'fatih/vim-go'
Plug 'neilagabriel/vim-geeknote'
Plug 'lambdalisue/vim-pyenv'
" Themes and colorschemes
Plug 'nanotech/jellybeans.vim'
Plug 'sjl/badwolf'
Plug 'chriskempson/base16-vim'
Plug 'fatih/molokai'
Plug 'reedes/vim-colors-pencil'
Plug 'morhetz/gruvbox'
Plug 'jordwalke/flatlandia'

call plug#end()

filetype indent off

set nobackup " Irrelevant these days
set number " Show linenumbers
let mapleader = "\<Space>" " Define leader key
set pastetoggle=<F2> " Quickly enable paste mode
set hidden " Don't moan about changes when switching buffers
set matchpairs=(:),{:},[:],<:> " Add <> to % matching

set modelines=5 " Enable modelines

set cpo=aABceFs$

set nowrap
set tabstop=4
set bs=2
set smarttab

nmap <leader>l :set list!<CR>
set listchars=tab:▸\ ,eol:¬,trail:-,
set fillchars+=vert:\│

set backupdir=/tmp//,.
set directory=/tmp//,.

au BufRead,BufNewFile *.py set expandtab
au BufRead,BufNewFile *.md,*.markdown setlocal textwidth=100
au BufRead,BufNewFile *.go setlocal noet ts=4 sw=4 sts=4

set hlsearch
set incsearch
set ignorecase

set wildmenu

filetype plugin on
set ai

vmap Q gq
nmap Q gqap

" Clear search
nmap <silent> ,/ :noh<cr>

au BufEnter *.sh if getline(1) == "" | :call setline(1, "#!/usr/bin/env bash") | endif
au BufEnter *.py if getline(1) == "" | :call setline(1, "#!/usr/bin/env python") | endif
au BufEnter *.rb if getline(1) == "" | :call setline(1, "#!/usr/bin/env ruby") | endif

set tags=./tags; " Tell vim to look upwards in the directory hierarchy for a tags file until it finds one

" Make vim deal with scoped identifiers instead of just hitting top-level
" modules when using ctags with Puppet code
set iskeyword=-,:,@,48-57,_,192-255
au FileType puppet setlocal isk+=:

" NeoVim specific
if has('nvim')
	set mouse=r
    nmap <BS> <C-w>h
	tnoremap <C-h> <C-\><C-n><C-w>h
    tnoremap <C-j> <C-\><C-n><C-w>j
    tnoremap <C-k> <C-\><C-n><C-w>k
    tnoremap <C-l> <C-\><C-n><C-w>l
endif
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

cmap w!! w !sudo tee % >/dev/null

map <F2> :mksession! .vim_session<CR>
map <F3> :source .vim_session<CR>

syntax on

" Appearance
set t_Co=256
set background=dark

" Theme related
let g:gruvbox_sign_column='dark0'
let g:gruvbox_bold=0

colorscheme gruvbox

" vim-airline
set laststatus=2
let g:airline_theme='gruvbox'
let g:airline_powerline_fonts=1
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
" let g:airline_left_sep = ''
" let g:airline_right_sep = ''
let g:airline_symbols.branch = '⭠'
let g:airline_symbols_readonly = '⭤'
let g:airline_symbols.linenr = '⭡'
let g:airline_symbols.paste = 'ρ'
let g:airline_detect_modified=1
let g:airline_detect_paste=1
let g:airline_inactive_collapse=0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1

" tmuxline
let g:tmuxline_preset = {
      \'a'    : '#S',
      \'win'  : '#I #W',
      \'cwin' : '#I #W',
      \'z'    : '#h',
	  \ 'options': {
	  \'status-justify': 'left'}
	  \}
let g:tmuxline_powerline_separators = 0

" NERDTree
map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowBookmarks=1
let NERDTreeShowHidden=1
let NERDTreeBookmarksSort = 1

" CtrlP settings
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = '/usr/local/bin/ag %s -l --nocolor --hidden -g ""'

" Buffer manipulation via sbd (smart buffer delete)
nnoremap <silent> <C-x> :Sbd<CR>
nnoremap <silent> <leader>bdm :Sbdm<CR>

" Silver Searcher
nnoremap <leader>s :Ag 

" Ultisnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsEditSplit="vertical"

" Syntastic configuration
let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
let g:syntastic_enable_ballons=has('ballon_eval')
let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_jump=1
let g:syntastic_auto_loc_list=1
let g:syntastic_loc_list_height=3

" Markdown stuff
nnoremap <leader>m :silent !open -a Marked.app '%:p'<cr>
let g:GeeknoteFormat="markdown"

" MacVim GUI overrides
if has("gui_macvim")
	set linespace=1
	set fuoptions=maxvert,maxhorz
	set guifont=Hasklig:h13
	set guioptions-=e " don't use gui tab apperance
	set guioptions-=T " hide toolbar
	set guioptions-=r " don't show scrollbars
	set guioptions-=l " don't show scrollbars
	set guioptions-=R " don't show scrollbars
	set guioptions-=L " don't show scrollbars
	set stal=2 " turn on tabs by default
	set gtl=%t gtt=%F " tab headings
	set macligatures
end
