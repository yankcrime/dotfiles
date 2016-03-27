" .vimrc
" nick@dischord.org

" {{{ vim-plug
call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'kien/ctrlp.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'scrooloose/nerdtree'
Plug 'fatih/vim-go'
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
Plug 'neilagabriel/vim-geeknote'
Plug 'lambdalisue/vim-pyenv'
Plug 'plasticboy/vim-markdown'
Plug 'junegunn/goyo.vim'
" Themes and colorschemes
Plug 'nanotech/jellybeans.vim'
Plug 'sjl/badwolf'
Plug 'chriskempson/base16-vim'
Plug 'reedes/vim-colors-pencil'
Plug 'fxn/vim-monochrome'
Plug 'croaker/mustang-vim'
Plug 'sts10/vim-mustard'

call plug#end()
" }}} end vim-plug
" {{{ General settings

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
au BufRead,BufNewFile *.go setlocal noet ts=4 sw=4 sts=4

set hlsearch
set incsearch
set ignorecase
" set cursorline

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

cmap w!! w !sudo tee % >/dev/null

map <F2> :mksession! .vim_session<CR>
map <F3> :source .vim_session<CR>

" Appearance
syntax on
set t_Co=256
set background=dark
let g:pencil_higher_contrast_ui = 0
let g:pencil_gutter_color = 1
colorscheme badwolf

" }}} End basic settings
" {{{ NeoVim
if has('nvim')
"	let $NVIM_TUI_ENABLE_TRUE_COLOR=1
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
"}}}
" {{{ Airline
set laststatus=2
let g:airline_powerline_fonts=1
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
" let g:airline_left_sep = ''
" let g:airline_right_sep = ''
"let g:airline_symbols.branch = '⭠'
"let g:airline_symbols_readonly = '⭤'
"let g:airline_symbols.linenr = '⭡'
"let g:airline_symbols.paste = 'ρ'
let g:airline_detect_modified=1
let g:airline_detect_paste=1
let g:airline_inactive_collapse=0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1
" let g:airline_theme = 'powerlineish'
" }}}
" {{{ tmuxline
let g:tmuxline_preset = {
      \'a'    : '#S',
      \'win'  : '#I #W',
      \'cwin' : '#I #W',
      \'z'    : '#h',
	  \ 'options': {
	  \'status-justify': 'left'}
	  \}
let g:tmuxline_powerline_separators = 1
" }}}
" {{{ NERDTree
map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowBookmarks=1
let NERDTreeShowHidden=1
let NERDTreeBookmarksSort = 1
" }}}
" {{{ CtrlP 
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = '/usr/local/bin/ag %s -l --nocolor --hidden -g ""'
" }}}
" {{{ Buffer manipulation via sbd (smart buffer delete)
nnoremap <silent> <C-x> :Sbd<CR>
nnoremap <silent> <leader>bdm :Sbdm<CR>
" }}}
" {{{ Silver Searcher
nnoremap <leader>s :Ag 
" }}}
" {{{ Ultisnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsEditSplit="vertical"
" }}}
" {{{ Syntastic
let g:syntastic_error_symbol='✗'
let g:syntastic_warning_symbol='⚠'
let g:syntastic_enable_ballons=has('ballon_eval')
let g:syntastic_always_populate_loc_list=1
let g:syntastic_auto_jump=1
let g:syntastic_auto_loc_list=1
let g:syntastic_loc_list_height=3
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
" }}}
" {{{ Markdown 
nnoremap <leader>m :silent !open -a Marked.app '%:p'<cr>
let g:GeeknoteFormat="markdown"
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_toc_autofit = 1
function! DistractionFreeWriting()
        colorscheme pencil
        set background=light
        set gfn=Cousine:h14
		set wrap linebreak textwidth=0 linespace=4 fullscreen
		Goyo 110x40
endfunction
" }}}
" {{{ Go
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
" }}}
" {{{ MacVim GUI overrides
if has("gui_macvim")
	set linespace=1
	set fuoptions=maxvert,maxhorz
	set guifont=PragmataPro:h14
	set guioptions-=e " don't use gui tab apperance
	set guioptions-=T " hide toolbar
	set guioptions-=r " don't show scrollbars
	set guioptions-=l " don't show scrollbars
	set guioptions-=R " don't show scrollbars
	set guioptions-=L " don't show scrollbars
	set stal=2 " turn on tabs by default
	set gtl=%t gtt=%F " tab headings
	set macligatures
	nnoremap <leader>word :call DistractionFreeWriting()<cr>
    end
" }}}
