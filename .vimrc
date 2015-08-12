" .vimrc
" nick@dischord.org

" vim-plug plugin manager
call plug#begin('~/.vim/plugged')

Plug 'gmarik/Vundle.vim'
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
Plug 'neilagabriel/vim-geeknote'
Plug 'airblade/vim-gitgutter'
Plug 'honza/vim-snippets'
Plug 'jmcantrell/vim-virtualenv'
Plug 'nanotech/jellybeans.vim'
Plug 'NLKNguyen/papercolor-theme'
Plug 'sjl/badwolf'

call plug#end()

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
au BufRead,BufNewFile *.yaml,*.yml so ~/.vim/after/syntax/yaml.vim
au BufRead,BufNewFile *.md,*.markdown setlocal textwidth=100

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

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

cmap w!! w !sudo tee % >/dev/null

map <F2> :mksession! .vim_session<CR>
map <F3> :source .vim_session<CR>

set background=dark
syntax on

set t_Co=256
colorscheme jellybeans

" vim-airline
set laststatus=2
let g:airline_theme='jellybeans'
let g:airline_powerline_fonts=1
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
if !has("gui_macvim")
  let g:airline_left_sep = ''
  let g:airline_right_sep = ''
end
let g:airline_symbols.branch = '⭠'
let g:airline_symbols_readonly = '⭤'
let g:airline_symbols.linenr = '⭡'
let g:airline_symbols.paste = 'ρ'
let g:airline_detect_modified=1
let g:airline_detect_paste=1
let g:airline_inactive_collapse=0
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1

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

" Ultisnips
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" Markdown stuff
nnoremap <leader>m :silent !open -a Marked.app '%:p'<cr>

" Geeknote settings
let g:GeeknoteFormat="markdown"
noremap <F8> :VirtualEnvActivate geeknote<CR>:Geeknote<CR>
nnoremap <leader>gn :Geeknote<cr>
nnoremap <leader>gcn :GeeknoteCreateNote

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
    set background=light
    colorscheme PaperColor
	let g:airline_theme='papercolor'
end
