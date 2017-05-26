" .vimrc
" nick@dischord.org

" {{{ vim-plug
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-sleuth'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'w0rp/ale', { 'for': ['puppet','go','yaml','python','ruby'] }
Plug 'godlygeek/tabular'
Plug 'cespare/vim-sbd'
Plug 'christoomey/vim-tmux-navigator'
Plug 'stephpy/vim-yaml', { 'for': ['yaml'] }
Plug 'fatih/vim-go', { 'for': ['go'] }
Plug 'rodjek/vim-puppet', { 'for': ['puppet'] }
Plug 'rking/ag.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'kien/ctrlp.vim', { 'on': [] }
Plug 'NLKNguyen/papercolor-theme'
Plug 'chriskempson/base16-vim'
Plug 'yankcrime/vim-colors-off'
Plug 'sonjapeterson/1989.vim'
Plug 'nanotech/jellybeans.vim'

call plug#end()

" }}} end vim-plug
" {{{ General

set nobackup " Irrelevant these days
set nonumber " Don't show linenumbers
let mapleader = "\<Space>" " Define leader key

set breakindent " indent wrapped lines, by...
set breakindentopt=shift:4,sbr " indenting them another level and showing 'showbreak' char
set showbreak=↪

set pastetoggle=<F2> " Quickly enable paste mode
set hidden " Don't moan about changes when switching buffers
set matchpairs=(:),{:},[:],<:> " Add <> to % matching

" Emoji completion with CTRL-X CTRL-U
set completefunc=emoji#complete

set modelines=5 " Enable modelines

set cursorline

set cpo=aABceFs$

set smarttab
set ttyfast

nmap <leader>l :set list!<CR>
set listchars=tab:▸\ ,eol:¬,trail:-,
set fillchars+=vert:\│

set backupdir=/tmp//,.
set directory=/tmp//,.

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

" Appearance
set t_Co=256
let g:jellybeans_use_term_italics=1
colorscheme off 
hi Normal ctermfg=none ctermbg=none
hi Statusline cterm=bold ctermfg=231 ctermbg=232
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
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END

augroup ft_ruby
    au!
    au Filetype ruby setlocal foldmethod=syntax
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
    au FileType muttrc setlocal foldmethod=marker foldmarker={{{,}}}
augroup END
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

" Default fzf layout
" - down / up / left / right
let g:fzf_layout = { 'down': '~40%' }

autocmd! User FzfStatusLine call <SID>fzf_statusline()

" }}}
" {{{ SBD (Smart Buffer Delete)
nnoremap <silent> <C-x> :Sbd<CR>
nnoremap <silent> <leader>bdm :Sbdm<CR>
" }}}
" {{{ Silver Searcher (Ag)
nnoremap <C-s> :Ag 
" }}}
" {{{ Markdown
nnoremap <leader>m :silent !open -a Marked.app '%:p'<cr>
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

set statusline=\ %<%.20F\ %h%w%m%r
set statusline+=%{SL('StatusGit')}
set statusline+=%#ErrorMsg#%{SL('SyntasticStatuslineFlag')}
set statusline+=%*%=%-14.(%r%y\ ⭡\ %l,%c%)\ %P\ 

" }}}
" {{{ ctags
" Workaround explicitly top-scoped Puppet classes / identifiers, i.e those
" prefixed with '::' which don't match to a file directly when used in
" conjunction with ctags
au FileType puppet nnoremap <c-]> :exe "tag " . substitute(expand("<cword>"), "^::", "", "")<CR>
au FileType puppet nnoremap <c-w><c-]> :tab split<CR>:exe "tag " . substitute(expand("<cword>"), "^::", "", "")<CR>
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
" {{{ GUI overrides
if has('gui_running')
    let g:ctrlp_map = '<C-p>'
    let g:ctrlp_match_window = 'bottom,order:ttb'
    let g:ctrlp_switch_buffer = 0
    let g:ctrlp_working_path_mode = 0
    let g:ctrlp_user_command = '/usr/local/bin/ag %s -l --nocolor --hidden -g ""'
    call plug#load('ctrlp.vim')
    set linespace=2
    set fuoptions=maxvert,maxhorz
    let macvim_skip_colorscheme=1
    set background=dark
    colorscheme off
    " hi Statusline guifg=#000000 guibg=#dddddd gui=bold
    set guifont=Operator\ Mono:h14
    set guioptions=e " don't use gui tab apperance
    set guioptions=T " hide toolbar
    set guioptions=r " don't show scrollbars
    set guioptions=l " don't show scrollbars
    set guioptions=R " don't show scrollbars
    set guioptions-=L " don't show scrollbars
    set gtl=%t gtt=%F " tab headings
end
" }}}

" vim:ts=4:sw=4:ft=vimrc:et
