" .vimrc
" nick@dischord.org

" {{{ vim-plug
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'w0rp/ale', { 'for': ['puppet','go','yaml','python','ruby'] }
Plug 'godlygeek/tabular'
Plug 'cespare/vim-sbd'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'christoomey/vim-tmux-navigator'
Plug 'plasticboy/vim-markdown'
Plug 'junegunn/vim-emoji'
Plug 'stephpy/vim-yaml', { 'for': ['yaml'] }
Plug 'fatih/vim-go', { 'for': ['go'] }
Plug 'rodjek/vim-puppet', { 'for': ['puppet'] }
Plug 'rking/ag.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'kien/ctrlp.vim', { 'on': [] }
Plug 'yankcrime/vim-colors-off'

call plug#end()

" }}} end vim-plug
" {{{ General

filetype indent off

set nobackup " Irrelevant these days
set nonumber " Don't show linenumbers
let mapleader = "\<Space>" " Define leader key

set pastetoggle=<F2> " Quickly enable paste mode
set hidden " Don't moan about changes when switching buffers
set matchpairs=(:),{:},[:],<:> " Add <> to % matching

" Emoji completion with CTRL-X CTRL-U
set completefunc=emoji#complete

set modelines=5 " Enable modelines

" set cursorline

set cpo=aABceFs$

set nowrap
set tabstop=4
set bs=2
set smarttab
set ttyfast

nmap <leader>l :set list!<CR>
set listchars=tab:▸\ ,eol:¬,trail:-,
" set fillchars+=vert:\│

set backupdir=/tmp//,.
set directory=/tmp//,.

au BufRead,BufNewFile *.py set expandtab
au BufRead,BufNewFile *.erb,*.rb,*.tf set expandtab ts=4 sw=4 ai
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
colorscheme off
set laststatus=2

" Fugitive shortcuts
noremap <leader>gadd :Gwrite<CR>
noremap <leader>gcommit :Gcommit<CR>
noremap <leader>gpush :Gpush<CR>
noremap <leader>gstat :Gstatus<CR>

nnoremap ; :

" }}} End basic settings
" {{{ Folding
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
" }}}
" {{{ BufTabLine
let g:buftabline_show=1
let g:buftabline_indicators=1
let g:buftabline_numbers=2
nmap 1 <Plug>BufTabLine.Go(1)
nmap 2 <Plug>BufTabLine.Go(2)
nmap 3 <Plug>BufTabLine.Go(3)
nmap 4 <Plug>BufTabLine.Go(4)
nmap 5 <Plug>BufTabLine.Go(5)
nmap 6 <Plug>BufTabLine.Go(6)
nmap 7 <Plug>BufTabLine.Go(7)
nmap 8 <Plug>BufTabLine.Go(8)
nmap 9 <Plug>BufTabLine.Go(9)
" }}}
" {{{ Tab management
noremap <leader>1 :tabnext 1<CR>
noremap <leader>2 :tabnext 2<CR>
noremap <leader>3 :tabnext 3<CR>
noremap <leader>4 :tabnext 4<CR>
noremap <leader>5 :tabnext 5<CR>
noremap <leader>6 :tabnext 6<CR>
noremap <leader>7 :tabnext 7<CR>
noremap <leader>8 :tabnext 8<CR>
noremap <leader>9 :tablast<CR>
" }}}
" {{{ NERDTree
map <C-n> :NERDTreeToggle<CR>
let g:NERDTreeShowBookmarks=1
let g:NERDTreeShowHidden=1
let g:NERDTreeBookmarksSort = 1
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
" }}}
" {{{ fzf
nnoremap <silent> <expr> <C-f> (expand('%') =~ 'NERD_tree' ? "\<c-w>\<c-w>" : '').":Files\<cr>"
nnoremap <silent> <C-b> :Buffers <CR>
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

" Default fzf layout
" - down / up / left / right
let g:fzf_layout = { 'down': '~40%' }

" In Neovim, you can set up fzf window using a Vim command
let g:fzf_layout = { 'window': 'enew' }
let g:fzf_layout = { 'window': '-tabnew' }

let g:fzf_buffers_jump = 1
let g:fzf_tags_command = 'ctags -R'

function! s:fzf_statusline()
  " Override statusline as you like
  highlight fzf1 ctermfg=161 ctermbg=251
  highlight fzf2 ctermfg=23 ctermbg=251
  highlight fzf3 ctermfg=237 ctermbg=251
  setlocal statusline=%#fzf1#\ >\ %#fzf2#fz%#fzf3#f
endfunction

autocmd! User FzfStatusLine call <SID>fzf_statusline()

" }}}
" {{{ Tmuxline
let g:tmuxline_powerline_separators = 0
let g:tmuxline_separators = {
    \ 'left' : '',
    \ 'left_alt': '',
    \ 'right' : '',
    \ 'right_alt' : '',
    \ 'space' : ' '}
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
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_toc_autofit = 1
au FileType markdown setlocal nonumber
au FileType markdown setlocal linebreak
au FileType markdown setlocal wrap
au FileType markdown setlocal ts=4 sw=4 sts=0 expandtab
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
let g:go_term_mode = "split"
" }}}
" {{{ Statusline

function! SL(function)
  if exists('*'.a:function)
    return call(a:function,[])
  else
    return ''
  endif
endfunction

function! StatusGit()
    let git = '⎇  ' . fugitive#head()
    return fugitive#head() != '' && winwidth('.') > 70 ? git : ''
endfunction

set statusline=[%n]
set statusline+=\ %<%.99f\ %h%w%m%r
set statusline+=%{SL('StatusGit')}
set statusline+=%#ErrorMsg#%{SL('SyntasticStatuslineFlag')}
set statusline+=%*%=%-14.(%r%y\ ⭡\ %l,%c%)\ %P
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
" {{{ Asyncrun
nnoremap <leader>ar :AsyncRun 
noremap <leader>arqf :call asyncrun#quickfix_toggle(8)<cr>
" }}}
" {{{ ALE
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 0
" }}}
" {{{ neovim
if has('nvim')
    nmap <BS> <C-w>h
    map <C-h> <C-w>h
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l
    tnoremap <leader><esc> <C-\><C-n>
    " nnoremap <bs> <c-w>h
    let g:terminal_scrollback_buffer_size = 10000
    let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
    set inccommand=nosplit
end
" }}}
" {{{ MacVim GUI overrides
if has('gui_running')
    let g:ctrlp_map = '<C-p>'
    let g:ctrlp_match_window = 'bottom,order:ttb'
    let g:ctrlp_switch_buffer = 0
    let g:ctrlp_working_path_mode = 0
    let g:ctrlp_user_command = '/usr/local/bin/ag %s -l --nocolor --hidden -g ""'
    call plug#load('ctrlp.vim')
    set linespace=2
    set transparency=5
    set fuoptions=maxvert,maxhorz
    let macvim_skip_colorscheme=1
    colorscheme off
    set guifont=Triplicate\ T4s:h14
    set guioptions=e " don't use gui tab apperance
    set guioptions=T " hide toolbar
    set guioptions=r " don't show scrollbars
    set guioptions=l " don't show scrollbars
    set guioptions=R " don't show scrollbars
    set guioptions-=L " don't show scrollbars
"    set stal=2 " turn on tabs by default
    set gtl=%t gtt=%F " tab headings
    " set macligatures
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

" vim:ts=4:sw=4:ft=vimrc:et
