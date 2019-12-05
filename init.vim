" neovim init.vim
" nick@dischord.org

" {{{ Plugins
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-dispatch'
Plug 'machakann/vim-sandwich'
Plug 'pearofducks/ansible-vim'
Plug 'godlygeek/tabular'
Plug 'cespare/vim-sbd'
Plug 'christoomey/vim-tmux-navigator'
Plug 'neoclide/coc.nvim', { 'branch': 'release', 'for': ['go', 'python', 'ansible', 'yaml'] }
Plug 'stephpy/vim-yaml', { 'for': ['yaml'] }
Plug 'fatih/vim-go', { 'for': ['go'] }
Plug 'rodjek/vim-puppet', { 'for': ['puppet'] }
Plug 'w0rp/ale', { 'for': ['puppet','go','yaml','python','ruby', 'ansible'] }
Plug 'justinmk/vim-dirvish'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'Lokaltog/vim-monotone'
Plug 'Lokaltog/neoranger'
Plug 'chriskempson/base16-vim'
Plug 'romainl/vim-sweet16'
Plug 'yankcrime/vim-colors-off'
Plug 'sjl/badwolf'
Plug 'robertmeta/nofrils'

call plug#end()

" }}} end vim-plug
" {{{ Statusline
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

" }}}
" {{{ General

set nobackup " Irrelevant these days
let mapleader = "\<Space>" " Define leader key
set noswapfile

set breakindent " indent wrapped lines, by...
set breakindentopt=shift:4,sbr " indenting them another level and showing 'showbreak' char
set showbreak=↪
set number
set noshowmode

set pastetoggle=<F2> " Quickly enable paste mode
set hidden " Don't moan about changes when switching buffers
set matchpairs=(:),{:},[:],<:> " Add <> to % matching

" Emoji completion with CTRL-X CTRL-U
set completefunc=emoji#complete

set modelines=5 " Enable modelines

" set cursorline

set cpo=aABceFs$

set smarttab
set ttyfast

nnoremap <leader>l :set list!<CR>
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
nnoremap Q gqap

" Clear search
nnoremap <silent> ,/ :noh<cr>

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

" use tab to cycle between windows
nnoremap <Tab> <C-w>w
nnoremap <S-Tab> <C-w>W

" appearance
set t_Co=256
set termguicolors

let iterm_profile = $ITERM_PROFILE

if iterm_profile == "Dark"
    set background=dark
    colorscheme monotone
    hi Normal gui=NONE guifg=NONE guibg=NONE ctermfg=none ctermbg=none
else
    set background=light        " Set solarized background color
    colorscheme nofrils-light
    hi Normal gui=NONE guifg=NONE guibg=NONE ctermfg=none ctermbg=none
    hi Statusline cterm=bold ctermbg=237 ctermfg=231 gui=bold
    hi Terminal ctermbg=none ctermfg=none
endif

hi clear SignColumn
set laststatus=2

" insert a datestamp at the top of a file
nnoremap <leader>N ggi# <C-R>=strftime("%Y-%m-%d - %A")<CR><CR><CR>

" super quick search and replace
nnoremap <Space><Space> :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>%       :%s/\<<C-r>=expand("<cword>")<CR>\>/

" fugitive shortcuts
noremap <leader>ga :Gwrite<CR>
noremap <leader>gc :Gcommit<CR>
noremap <leader>gp :Gpush<CR>
noremap <leader>gs :Gstatus<CR>

" convenience remap - one less key to press
nnoremap ; :

" juggling with quickfix entries
nnoremap <End>  :cnext<CR>
nnoremap <Home> :cprevious<CR>

" juggling with buffers
nnoremap <PageUp>   :bprevious<CR>
nnoremap <PageDown> :bnext<CR>
nnoremap <BS>       :buffer#<CR>

" super quick search and replace
nnoremap <Space><Space> :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>%       :%s/\<<C-r>=expand("<cword>")<CR>\>/

" do some sensible things with listings
cnoremap <expr> <CR> <SID>CCR()
function! s:CCR()
	command! -bar Z silent set more|delcommand Z
	if getcmdtype() == ":"
		let cmdline = getcmdline()
		    if cmdline =~ '\v\C^(dli|il)' | return "\<CR>:" . cmdline[0] . "jump   " . split(cmdline, " ")[1] . "\<S-Left>\<Left>\<Left>"
		elseif cmdline =~ '\v\C^(cli|lli)' | return "\<CR>:silent " . repeat(cmdline[0], 2) . "\<Space>"
		elseif cmdline =~ '\C^changes' | set nomore | return "\<CR>:Z|norm! g;\<S-Left>"
		elseif cmdline =~ '\C^ju' | set nomore | return "\<CR>:Z|norm! \<C-o>\<S-Left>"
		elseif cmdline =~ '\v\C(#|nu|num|numb|numbe|number)$' | return "\<CR>:"
		elseif cmdline =~ '\C^ol' | set nomore | return "\<CR>:Z|e #<"
		elseif cmdline =~ '\v\C^(ls|files|buffers)' | return "\<CR>:b"
		elseif cmdline =~ '\C^marks' | return "\<CR>:norm! `"
		elseif cmdline =~ '\C^undol' | return "\<CR>:u "
		else | return "\<CR>" | endif
	else | return "\<CR>" | endif
endfunction

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
" {{{ fzf (and Ripgrep)
nnoremap <silent> <C-f> :Files <CR>
nnoremap <silent> <C-b> :Buffers <CR>
nnoremap <silent> <C-t> :call fzf#vim#tags(expand('<cword>'))<cr>
nnoremap <silent> <C-s> :Rg <CR>

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

" In Neovim, you can set up fzf window using a Vim command
let g:fzf_layout = { 'window': 'enew' }
let g:fzf_layout = { 'window': '-tabnew' }

let g:fzf_buffers_jump = 1
let g:fzf_tags_command = 'ctags -R'

" Default fzf layout
" - down / up / left / right
let g:fzf_layout = { 'down': '~40%' }

command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=never --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

" }}}
" {{{ Golang
au FileType go nnoremap <leader>r <Plug>(go-run)
au FileType go nnoremap <leader>b <Plug>(go-build)
au FileType go nnoremap <leader>t <Plug>(go-test)
au FileType go nnoremap <leader>c <Plug>(go-coverage)
au FileType go nnoremap <Leader>rv <Plug>(go-run-vertical)
au FileType go nnoremap <Leader>gd <Plug>(go-doc)
au FileType go nnoremap <Leader>gv <Plug>(go-doc-vertical)
au BufNewFile,BufRead *.go setlocal noet ts=4 sw=4 sts=4
let g:go_term_enabled = 1
let g:go_term_mode = "split"
" }}}
" {{{ Neovim
if has('nvim')
    nnoremap <BS> <C-w>h
"    tnoremap <C-h> <C-\><C-N><C-w>h
"    tnoremap <C-j> <C-\><C-N><C-w>j
"    tnoremap <C-k> <C-\><C-N><C-w>k
"    tnoremap <C-l> <C-\><C-N><C-w>l
    tnoremap <leader><esc> <C-\><C-n>
    au TermOpen * setlocal nonumber norelativenumber
    let g:terminal_scrollback_buffer_size = 10000
    let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
    set inccommand=nosplit
    command! -nargs=* T split | terminal <args>
    command! -nargs=* VT vsplit | terminal <args>
    nnoremap <leader>t :T<cr>

    let g:terminal_color_0  = '#000000'
    let g:terminal_color_1  = '#e65544'
    let g:terminal_color_2  = '#4ca24a'
    let g:terminal_color_3  = '#dbc18e'
    let g:terminal_color_4  = '#3c74f5'
    let g:terminal_color_5  = '#a726a5'
    let g:terminal_color_6  = '#4ca24a'
    let g:terminal_color_7  = '#c7c7c7'
    let g:terminal_color_8  = '#000000'
    let g:terminal_color_9  = '#ef2929'
    let g:terminal_color_10 = '#4ca24a'
    let g:terminal_color_11 = '#dbc18e'
    let g:terminal_color_12 = '#3c74f5'
    let g:terminal_color_13 = '#b127a5'
    let g:terminal_color_14 = '#4ca24a'
    let g:terminal_color_15 = '#9c9c9c'
end
" }}}
" {{{ Ansible
" Fix annoying and often wrong reindentation
set indentkeys-=<:>
let g:ansible_yamlKeyName = 'yamlKey'
" }}}
" {{{ Golang
au FileType go nnoremap <leader>r <Plug>(go-run)
au FileType go nnoremap <leader>b <Plug>(go-build)
au FileType go nnoremap <leader>t <Plug>(go-test)
au FileType go nnoremap <leader>c <Plug>(go-coverage)
au FileType go nnoremap <Leader>rv <Plug>(go-run-vertical)
au FileType go nnoremap <Leader>gd <Plug>(go-doc)
au FileType go nnoremap <Leader>gv <Plug>(go-doc-vertical)
au BufNewFile,BufRead *.go setlocal noet ts=4 sw=4 sts=4
let g:go_term_enabled = 1
let g:go_term_mode = "split"
" }}}
" {{{ Markdown
nnoremap <leader>m :silent !open -a Marked 2.app '%:p'<cr>
" }}}
" {{{ SBD (Smart Buffer Delete)
nnoremap <silent> <C-x> :Sbd<CR>
nnoremap <silent> <leader>bdm :Sbdm<CR>
" }}}
" {{{ Ctags
" Workaround explicitly top-scoped Puppet classes / identifiers, i.e those
" prefixed with '::' which don't match to a file directly when used in
" conjunction with ctags
au FileType puppet nnoremap <c-]> :exe "tag " . substitute(expand("<cword>"), "^::", "", "")<CR>
au FileType puppet nnoremap <c-w><c-]> :tab split<CR>:exe "tag " . substitute(expand("<cword>"), "^::", "", "")<CR>
" }}}
" {{{ ALE
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 0
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_sign_warning='●'
hi ALEErrorSign ctermfg=red ctermbg=none
let g:ale_sign_error='●'
hi ALEWarningSign ctermfg=yellow ctermbg=none
" }}}
" {{{ COC
" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <S-Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

inoremap <expr> <Tab> pumvisible() ? "\<C-y>" : "\<C-g>u\<Tab>"

" }}}

" vim:ts=4:sw=4:ft=vimrc:et
