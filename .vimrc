" .vimrc
" nick@dischord.org

" {{{ vim-plug
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'kien/ctrlp.vim'
Plug 'rking/ag.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'godlygeek/tabular'
Plug 'airblade/vim-gitgutter'
Plug 'fatih/vim-go'
Plug 'rodjek/vim-puppet'
Plug 'cespare/vim-sbd'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'christoomey/vim-tmux-navigator'
Plug 'stephpy/vim-yaml'
Plug 'lambdalisue/vim-pyenv'
Plug 'plasticboy/vim-markdown'
Plug 'majutsushi/tagbar'
Plug 'ap/vim-buftabline'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/vim-emoji'
" Themes and colorschemes
Plug 'godlygeek/csapprox'
Plug 'nanotech/jellybeans.vim'
Plug 'sjl/badwolf'
Plug 'chriskempson/base16-vim'
Plug 'reedes/vim-colors-pencil'
Plug 'fxn/vim-monochrome'
Plug 'cocopon/iceberg.vim'

call plug#end()
" }}} end vim-plug
" {{{ General

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
set ttyfast

nmap <leader>l :set list!<CR>
set listchars=tab:▸\ ,eol:¬,trail:-,
set fillchars+=vert:\│

set backupdir=/tmp//,.
set directory=/tmp//,.

au BufRead,BufNewFile *.py set expandtab
au BufRead,BufNewFile *.erb,*.rb,*.tf set expandtab ts=4 sw=4 ai
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

autocmd BufRead ~/.mutt/tmp/*      :source ~/.mutt/mail.vim

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
set background=light
colorscheme direwolf
" set statusline=%<\ %n:%f\ %m%r%y%=%-35.(line:\ %l\ of\ %L,\ col:\ %c%V\ (%P)%)

function! SL(function)
  if exists('*'.a:function)
    return call(a:function,[])
  else
    return ''
  endif
endfunction

function! StatusGit()
    let git = '⎇ ' . fugitive#head()
    return fugitive#head() != '' && winwidth('.') > 70 ? git : ''
endfunction

set statusline=[%n]
set statusline+=\ ▶\ %<%.99f\ %h%w%m%r
set statusline+=%{SL('StatusGit')}
set statusline+=%#ErrorMsg#%{SL('SyntasticStatuslineFlag')}
set statusline+=%*%=%-14.(%r%y\ ⭡\ %l,%c%)\ %P\ 

silent! if emoji#available()
  function! S_modified()
    if &modified
      return emoji#for('kiss').' '
    elseif !&modifiable
      return emoji#for('construction').' '
    else
      return ''
    endif
  endfunction

  function! S_fugitive()
    if !exists('g:loaded_fugitive')
      return ''
    endif
    let head = fugitive#head()
    if empty(head)
      return ''
    else
      return head == 'master' ? emoji#for('crown') : emoji#for('seedling').' '.head
    endif
  endfunction

  function! S_filetype()
    if empty(&filetype)
      return emoji#for('grey_question')
    else
      return get(s:ft_emoji, &filetype, '['.&filetype.']')
    endif
  endfunction

  function! MyStatusLine()
    let mod = '%{S_modified()}'
    let ro  = "%{&readonly ? emoji#for('lock') . ' ' : ''}"
    let ft  = '%y'
    let fug = ' %{S_fugitive()}'
    let sep = ' %= '
    let pos = ' ⭡ %l,%c%V '
    let pct = ' %P '
    return '[%n] %f %<'.mod.ro.ft.fug.sep.pos.pct
  endfunction

  set statusline=%!MyStatusLine()
endif

set laststatus=2

set completefunc=emoji#complete

" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za

augroup ft_vim
    au!
    au FileType vim setlocal foldmethod=marker keywordprg=:help
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END

augroup ft_ruby
    au!
    au Filetype ruby setlocal foldmethod=syntax
	au Filetype ruby set expandtab ts=4 sw=4 ai
    au BufRead,BufNewFile Capfile setlocal filetype=ruby
augroup END

augroup ft_puppet
    au!
    au Filetype puppet setlocal foldmethod=marker
    au Filetype puppet setlocal foldmarker={,}
augroup END

augroup ft_muttrc
    au!
    au BufRead,BufNewFile *.muttrc set ft=muttrc
	au Filetype muttrc set expandtab ts=4 sw=4 ai
    au FileType muttrc setlocal foldmethod=marker foldmarker={{{,}}}
augroup END

" }}} End basic settings
" {{{ MacVim GUI overrides
if has("gui_macvim")
    set linespace=2
    set fuoptions=maxvert,maxhorz
    set guifont=Triplicate\ T4s:h14
    set guioptions-=e " don't use gui tab apperance
    set guioptions-=T " hide toolbar
    set guioptions-=r " don't show scrollbars
    set guioptions-=l " don't show scrollbars
    set guioptions-=R " don't show scrollbars
    set guioptions-=L " don't show scrollbars
    set stal=2 " turn on tabs by default
    set gtl=%t gtt=%F " tab headings
    " set macligatures
    nnoremap <leader>word :call DistractionFreeWriting()<cr>
    nmap <D-1> <Plug>BufTabLine.Go(1)
    nmap <D-2> <Plug>BufTabLine.Go(2)
    nmap <D-3> <Plug>BufTabLine.Go(3)
    nmap <D-4> <Plug>BufTabLine.Go(4)
    nmap <D-5> <Plug>BufTabLine.Go(5)
    nmap <D-6> <Plug>BufTabLine.Go(6)
    nmap <D-7> <Plug>BufTabLine.Go(7)
    nmap <D-8> <Plug>BufTabLine.Go(8)
    nmap <D-9> <Plug>BufTabLine.Go(9)
    nmap <D-0> <Plug>BufTabLine.Go(10)
    end
" }}}
" {{{ BufTabLine
let g:buftabline_show=1
let g:buftabline_indicators=1
let g:buftabline_numbers=2
nmap <A-1> <Plug>BufTabLine.Go(1)
nmap <A-2> <Plug>BufTabLine.Go(2)
nmap <A-3> <Plug>BufTabLine.Go(3)
nmap <A-4> <Plug>BufTabLine.Go(4)
nmap <A-5> <Plug>BufTabLine.Go(5)
nmap <A-6> <Plug>BufTabLine.Go(6)
nmap <A-7> <Plug>BufTabLine.Go(7)
nmap <A-8> <Plug>BufTabLine.Go(8)
nmap <A-9> <Plug>BufTabLine.Go(9)
nmap <A-0> <Plug>BufTabLine.Go(10)
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
" {{{ SBD (Smart Buffer Delete)
nnoremap <silent> <C-x> :Sbd<CR>
nnoremap <silent> <leader>bdm :Sbdm<CR>
" }}}
" {{{ Silver Searcher (Ag)
nnoremap <C-s> :Ag 
" }}}
" {{{ Ultisnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsEditSplit="vertical"
" }}}
" {{{ Syntastic
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
au FileType go nmap <Leader>rv <Plug>(go-run-vertical)
au FileType go nmap <Leader>gd <Plug>(go-doc)
au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
let g:go_term_enabled = 1
" }}}
" {{{ ctags / Tagbar
" Workaround explicitly top-scoped Puppet classes / identifiers, i.e those
" prefixed with '::' which don't match to a file directly when used in
" conjunction with ctags
nnoremap <C-t> :TagbarToggle<CR>
au FileType puppet nnoremap <c-]> :exe "tag " . substitute(expand("<cword>"), "^::", "", "")<CR>  
au FileType puppet nnoremap <c-w><c-]> :tab split<CR>:exe "tag " . substitute(expand("<cword>"), "^::", "", "")<CR>
let g:tagbar_type_puppet = {
  \ 'ctagstype': 'puppet',
  \ 'kinds': [
    \'c:class',
    \'s:site',
    \'n:node',
    \'d:definition',
    \'r:resource',
    \'f:default'
  \]
\}
" }}}
