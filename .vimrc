" .vimrc
" nick@dischord.org

" {{{ Plugins
silent! call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-vinegar'
Plug 'machakann/vim-sandwich'
Plug 'cespare/vim-sbd'
Plug 'stephpy/vim-yaml', { 'for': ['yaml'] }
Plug 'pearofducks/ansible-vim', { 'branch': 'v2', 'for': ['yaml.ansible'] }
Plug 'fatih/vim-go', { 'for': ['go'] }
Plug 'w0rp/ale', { 'for': ['puppet','ansible','yaml','python','go','ruby'] }
Plug 'rking/ag.vim'
Plug 'justinmk/vim-dirvish'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'jszakmeister/vim-togglecursor'
Plug 'hashivim/vim-terraform', { 'for': ['terraform'] }
Plug 'robertmeta/nofrils'

call plug#end()

" }}} end vim-plug
" {{{ General

set nobackup " Irrelevant these days
let mapleader = "\<Space>" " Define leader key
set noswapfile

set breakindent " indent wrapped lines, by...
set breakindentopt=shift:4,sbr " indenting them another level and showing 'showbreak' char
set showbreak=↪

set number

set hidden " Don't moan about changes when switching buffers
set matchpairs=(:),{:},[:],<:> " Add <> to % matching

set modelines=5

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

" formatting shortcuts
vmap Q gq
nnoremap Q gqap

" clear search
nnoremap <silent> ,/ :noh<cr>

au BufEnter *.sh if getline(1) == "" | :call setline(1, "#!/usr/bin/env bash") | endif
au BufEnter *.py if getline(1) == "" | :call setline(1, "#!/usr/bin/env python") | endif
au BufEnter *.rb if getline(1) == "" | :call setline(1, "#!/usr/bin/env ruby") | endif

set tags=./tags; " Tell vim to look upwards in the directory hierarchy for a tags file until it finds one

cmap w!! w !sudo tee % >/dev/null

" appearance
" set cursorline
set t_Co=256
set termguicolors
colorscheme nofrils-light

hi Normal gui=NONE guifg=NONE guibg=NONE ctermfg=none ctermbg=none
hi Statusline cterm=bold ctermbg=237 ctermfg=231 gui=bold
" hi StatusLineNC guifg=#555555 guibg=NONE gui=underline
hi Terminal ctermbg=none ctermfg=none
" hi StatuslineTerm ctermbg=237 ctermfg=231 gui=bold
" hi StatuslineTermNC term=reverse ctermfg=243 ctermbg=236 guifg=#767676 guibg=#303030
hi clear SignColumn
set laststatus=2

" super quick search and replace
nnoremap <Space><Space> :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>%       :%s/\<<C-r>=expand("<cword>")<CR>\>/

" terminal stuff
tnoremap <leader><Esc> <C-W>N

" }}} End general settings
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
" {{{ FZF
nnoremap <silent> <C-f> :Files <CR>
nnoremap <silent> <C-b> :Buffers <CR>
nnoremap <silent> <C-t> :call fzf#vim#tags(expand('<cword>'))<cr>

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

" In Neovim, you can set up fzf window using a Vim command
let g:fzf_layout = { 'window': 'enew' }
let g:fzf_layout = { 'window': '-tabnew' }

let g:fzf_buffers_jump = 1
let g:fzf_tags_command = 'ctags -R'

" Workaround for https://github.com/junegunn/fzf/issues/809
let $FZF_DEFAULT_OPTS .= ' --no-height'

" Default fzf layout
" - down / up / left / right
let g:fzf_layout = { 'down': '~40%' }

command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

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
" {{{ Ansible
" Fix annoying and often wrong reindentation
set indentkeys-=<:>
let g:ansible_yamlKeyName = 'yamlKey'
" }}}
" {{{ SBD (Smart Buffer Delete)
nnoremap <silent> <C-x> :Sbd<CR>
nnoremap <silent> <leader>bdm :Sbdm<CR>
" }}}
" {{{ Markdown
nnoremap <leader>m :silent !open -a Marked 2.app '%:p'<cr>
" }}}
" {{{ Silver Searcher (Ag)
function! AGSearch() abort
    call inputsave()
    let searchterm = input('Search string: ')
    call inputrestore()
    execute 'Ag' searchterm
endfunction
nnoremap <C-s> :call AGSearch()<cr>
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
" {{{ Neovim
if has('nvim')
    nnoremap <BS> <C-w>h
"    tnoremap <C-h> <C-\><C-N><C-w>h
"    tnoremap <C-j> <C-\><C-N><C-w>j
"    tnoremap <C-k> <C-\><C-N><C-w>k
"    tnoremap <C-l> <C-\><C-N><C-w>l
    tnoremap <esc><esc> <C-\><C-n>
    au TermOpen * setlocal nonumber norelativenumber
    let g:terminal_scrollback_buffer_size = 10000
    let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
    set inccommand=nosplit
    command! -nargs=* T split | terminal <args>
    command! -nargs=* VT vsplit | terminal <args>
    nnoremap <leader>t :T<cr>
end
" }}}

" vim:ts=4:sw=4:ft=vimrc:et
