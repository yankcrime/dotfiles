" .vimrc
" nick@dischord.org

" {{{ vim-plug
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'rking/ag.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'w0rp/ale', { 'for': ['puppet','go','yaml','python','ruby'] }
Plug 'godlygeek/tabular'
Plug 'airblade/vim-gitgutter'
Plug 'cespare/vim-sbd'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'christoomey/vim-tmux-navigator'
Plug 'plasticboy/vim-markdown'
Plug 'junegunn/vim-emoji'
Plug 'stephpy/vim-yaml', { 'for': ['yaml'] }
Plug 'fatih/vim-go', { 'for': ['go'] }
Plug 'rodjek/vim-puppet', { 'for': ['puppet'] }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'kien/ctrlp.vim', { 'on': [] }
" Appearance
Plug 'itchyny/lightline.vim'
" Plug 'ryanoasis/vim-devicons'
" Plug 'bagrat/vim-workspace'
Plug 'edkolev/tmuxline.vim'
Plug 'sjl/badwolf'
Plug 'yankcrime/vim-colors-off'
Plug 'yankcrime/direwolf'
Plug 'cocopon/iceberg.vim'
Plug 'chriskempson/base16-vim'
Plug 'sonjapeterson/1989.vim'

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

" Emoji completion with CTRL-X CTRL-U
set completefunc=emoji#complete

set modelines=5 " Enable modelines

set cursorline

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
set background=light
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
" {{{ NERDTree
map <C-n> :NERDTreeToggle<CR>
let g:NERDTreeShowBookmarks=1
let g:NERDTreeShowHidden=1
let g:NERDTreeBookmarksSort = 1
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
let g:NERDTreeDirArrowExpandable="\uf460"
let g:NERDTreeDirArrowCollapsible="\uf47c"
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

" Customize fzf colors to match your color scheme
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'NONE'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

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
let g:ale_sign_column_always = 1
let g:ale_sign_error = "\uf06a"
let g:ale_sign_warning = "\uf071"

let g:ale_statusline_format = ["\uf06a %d", "\uf071 %d", "\uf058 "]

let g:ale_echo_msg_error_str = "\uf06a"
let g:ale_echo_msg_warning_str = "\uf071"
let g:ale_echo_msg_format = " \uf0ad %linter% \ue216  %severity% %s"
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
" {{{ Lightline
let g:lightline = {}
let g:lightline.enable = {}
let g:lightline.enable.statusline = 1 
let g:lightline.enable.tabline = 0

let g:lightline.separator = {}
let g:lightline.separator.left = "\ue0b0"
let g:lightline.separator.right = "\ue0b2"
" let g:lightline.separator.right = ""
let g:lightline.subseparator = {}
let g:lightline.subseparator.left = "\ue0b1"
" let g:lightline.subseparator.right = ""
let g:lightline.subseparator.right = "\ue0b3"

let g:lightline.inactive = {}
let g:lightline.inactive.left = [
            \   ['nerdtree_icon'],
            \   ['filename', 'modified']
            \ ]
let g:lightline.inactive.right = []

let g:lightline.active = {}
let g:lightline.active.left = [
            \   ['nerdtree_icon', 'ctrlp_icon', 'mode', 'paste'],
            \   ['filename', 'ctrlp_mode'],
            \   ['ctrlp_next', 'modified', 'branch'],
            \ ]
" TODO: Add virtual env
let g:lightline.active.right = [
            \   ['vim_icon'],
            \   ['alenumbers'],
            \]

let g:lightline.component = {}
let g:lightline.component.blank = "%{\"\ue62b\"}"

let g:lightline.component_function = {}
let g:lightline.component_function.paste = 'LightLinePaste'
let g:lightline.component_function.modified = 'LightLineModified'
let g:lightline.component_function.filename = 'LightLineFilename'
let g:lightline.component_function.mode = 'LightLineMode'
let g:lightline.component_function.branch= 'LightLineBranch'
let g:lightline.component_function.nerdtree_icon = 'LightLineNerdTreeIcon'
let g:lightline.component_function.ctrlp_icon = 'LightLineCtrlpIcon'
let g:lightline.component_function.ctrlp_mode = 'LightLineCtrlpMode'
let g:lightline.component_function.ctrlp_next = 'LightLineCtrlpNext'
let g:lightline.component_function.alenumbers = 'LightLineAleNumbers'
let g:lightline.component_function.vim_icon = 'LightLineVimIcon'

function! LightLineVimIcon()
    if winwidth(0) > 20
        return "\ue62b"
    endif

    return ''
endfunction

function! LightLineModified()
    let fname = expand('%:t')

    if fname =~ "NERD_tree"
        return ''
    endif

    if fname =~ "ControlP"
        return ''
    endif

    return &modified ? "\uf0c7 " : &modifiable ? &readonly ? "\uf13e " : '' : "\uf023 "
endfunction

function! LightLinePaste()
    let fname = expand('%:t')

    if fname =~ "NERD_tree"
        return ''
    endif

    if fname =~ "ControlP"
        return ''
    endif

    return &paste ? "\uf0ea" : ''
endfunction

function! LightLineFilename()
    let fname = expand('%:t')

    if fname =~ 'ControlP'
        return ''
    endif
    
    if fname =~ 'NERD_tree'
        if winwidth(0) > 17
            return 'Project'
        endif
        return ''
    endif

    if fname == ''
        return "\uf069"
    endif

    let icon = ''
    if exists('*WebDevIconsGetFileTypeSymbol')
        let icon = WebDevIconsGetFileTypeSymbol(fname)
    endif

    return icon . fname
endfunction

function! LightLineMode()
    let fname = expand('%:t')

    if fname =~ 'NERD_tree'
        return ''
    endif

    if fname =~ 'ControlP'
        return ''
    endif

    if winwidth(0) > 50
        return lightline#mode()
    else
        return lightline#mode()[0]
    endif

endfunction

function! LightLineBranch()
    if !exists('*fugitive#head')
        return ''
    endif
    let fname = expand('%:t')

    if fname =~ 'NERD_tree'
        return ''
    endif

    let branch = fugitive#head()

    if !strlen(branch)
        return ''
    endif

    return "\ue0a0 " . branch
endfunction

function! LightLineNerdTreeIcon()
    let fname = expand('%:t')

    if fname =~ 'NERD_tree'
        if winwidth(0) > 9
            return "\ue257"
        endif
        return ''
    endif

    return ''
endfunction

function! LightLineCtrlpIcon()
    let fname = expand('%:t')

    if fname =~ 'ControlP'
        return "\uf002"
    endif

    return ''
endfunction

let s:ctrlp_mode_map = {
            \   "mru": "\uf017 mru",
            \   "mru files": "\uf017 mru",
            \   "files": "\uf016 files",
            \   "fil": "\uf016 files",
            \   "buf": "\uf15c buffers",
            \   "buffers": "\uf15c buffers",
            \}

function! LightLineCtrlpMode()
    let fname = expand('%:t')

    if fname =~ 'ControlP'
        let current_mode = get(s:ctrlp_mode_map, g:lightline.ctrlp_item, g:lightline.ctrlp_item)

        if exists('current_mode')
            return current_mode
        else
            ''
        endif
    endif

    return ''
endfunction

function! LightLineCtrlpNext()
    let fname = expand('%:t')

    if fname =~ 'ControlP'
        let next_mode = get(s:ctrlp_mode_map, g:lightline.ctrlp_next, g:lightline.ctrlp_next)
        let prev_mode = get(s:ctrlp_mode_map, g:lightline.ctrlp_prev, g:lightline.ctrlp_prev)
        let modes = next_mode . ' ' . g:lightline.subseparator.left . ' ' . prev_mode

        if exists('modes')
            return modes
        else
            ''
        endif
    endif

    return ''
endfunction

function! LightLineAleNumbers()
    if !exists('*ALEGetStatusLine')
        return ''
    endif

    let fname = expand('%:t')

    if fname =~ 'NERD_tree'
        return ''
    endif

    if fname =~ 'ControlP'
        return ''
    endif

    return ALEGetStatusLine()
endfunction


" CtrlP scpeific settings
let g:ctrlp_status_func = {
  \ 'main': 'CtrlPStatusFunc_1',
  \ 'prog': 'CtrlPStatusFunc_2',
  \ }

function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
    let g:lightline.ctrlp_prev = a:prev
    let g:lightline.ctrlp_item = a:item
    let g:lightline.ctrlp_next = a:next
    return lightline#statusline(0)
endfunction

function! CtrlPStatusFunc_2(str)
    return lightline#statusline(0)
endfunction
" }}}
"" {{{ Bufferline
"let g:workspace_powerline_separators = 1
"let g:workspace_tab_icon = "\uf00a"
"let g:workspace_left_trunc_icon = "\uf0a8"
"let g:workspace_right_trunc_icon = "\uf0a9"
"noremap <Tab> :WSNext<CR>
"noremap <S-Tab> :WSPrev<CR>
"noremap <Leader><Tab> :WSClose<CR>
"noremap <Leader><S-Tab> :WSClose!<CR>
"noremap <C-t> :WSTabNew<CR>
"
"cabbrev bonly WSBufOnly
"
"highlight WorkspaceFill ctermfg=240 ctermbg=235 guifg=#585858 guibg=#262626
"highlight WorkspaceBufferCurrent ctermfg=235 ctermbg=148 guifg=#262626 guibg=#afdf00
"highlight WorkspaceBufferActive ctermfg=252 ctermbg=240 guifg=#d0d0d0 guibg=#585858
"highlight WorkspaceBufferHidden ctermfg=235 ctermbg=240 guifg=#262626 guibg=#585858
"highlight WorkspaceTabCurrent ctermfg=22 ctermbg=148 guifg=#005f00 guibg=#afdf00
"" }}}
" vim:ts=4:sw=4:ft=vimrc:et
