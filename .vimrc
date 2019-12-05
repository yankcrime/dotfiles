" .vimrc
" nick@dischord.org

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

autocmd FileType json syntax match Comment +\/\/.\+$+

set tags=./tags; " Tell vim to look upwards in the directory hierarchy for a tags file until it finds one

cmap w!! w !sudo tee % >/dev/null

" appearance
syntax on
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

" super quick search and replace
nnoremap <Space><Space> :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>%       :%s/\<<C-r>=expand("<cword>")<CR>\>/

" terminal stuff
tnoremap <leader><Esc> <C-W>N

" }}} End general settings
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
" {{{ Ctags
" Workaround explicitly top-scoped Puppet classes / identifiers, i.e those
" prefixed with '::' which don't match to a file directly when used in
" conjunction with ctags
au FileType puppet nnoremap <c-]> :exe "tag " . substitute(expand("<cword>"), "^::", "", "")<CR>
au FileType puppet nnoremap <c-w><c-]> :tab split<CR>:exe "tag " . substitute(expand("<cword>"), "^::", "", "")<CR>
" }}}
" {{{ ALE
let g:ale_lint_on_enter = 1
let g:ale_lint_on_insert_leave = 0
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_sign_warning='●'
hi ALEErrorSign ctermfg=red ctermbg=none
let g:ale_sign_error='●'
hi ALEWarningSign ctermfg=yellow ctermbg=none
" }}}

" vim:ts=4:sw=4:ft=vimrc:et
