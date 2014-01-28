" .vimrc
" nick@dischord.org

" Pathogen for loading vim plugins from ~/.vim/bundle
execute pathogen#infect()

" Irrelevant these days
set nobackup

" Show linenumbers
set number

" Quickly enable paste mode 
set pastetoggle=<F2>

" Don't moan about changes when switching buffers
set hidden

" Add <> to % matching
set matchpairs=(:),{:},[:],<:>

" Set background = dark makes all colors show up brighter
" Also, set syntax on whenever possible
set background=dark
syntax on

" See :help cpo for flags-explanation $ is that when you do cw on a word, a $
" appears at the end an you can type over the word. All the others are
" vim-default. See help.
set cpo=aABceFs$

" Do not auto wrap lines
" Set the size of a tab to match 4 spaces
" Set backspace type as 2 
set nowrap
set tabstop=4
set bs=2

" For Python's .py files
au BufRead,BufNewFile *.py set expandtab

" When searching highlight and keep highlighted, the words you search for
set hlsearch
set incsearch
set ignorecase

" When reading files or doing actions on files, press TAB to show a
" browseable menu
set wildmenu

" Enable filetype plugins so auto-wrapping works for mail and such but
" Disable indent plugin
filetype plugin on
filetype indent off
set ai

" Justify the paragraph
vmap Q gq
nmap Q gqap

" Clear highlighted searches by doing ,/
" nmap <silent> ,/ :let @/=""<CR>
nmap <silent> ,/ :noh<cr>

" automatically insert shebangs in certain files
au BufEnter *.sh if getline(1) == "" | :call setline(1, "#!/usr/bin/env bash") | endif 
au BufEnter *.py if getline(1) == "" | :call setline(1, "#!/usr/bin/env python") | endif 
au BufEnter *.rb if getline(1) == "" | :call setline(1, "#!/usr/bin/env ruby") | endif 

" Quotes unquoted HTML tag properties throughout buffer.
" map <F9> :%s/\([^&^?]\)\(\<[[:alnum:]-]\{-}\)=\([[:alnum:]-#%]\+\)/\1\2="\3"/g<Return>

" Ad-hoc C development while we're not using makefiles
set makeprg=gcc\ -o\ %<\ %

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Very handy option to write a file that we've forgotten to open via sudo
" Just do w!! instead
cmap w!! w !sudo tee % >/dev/null

" Quick write session with F2 
map <F2> :mksession! .vim_session<CR>
" And load session with F3
map <F3> :source .vim_session<CR>

set t_Co=256 
colorscheme badwolf

" OSX Specific *****************************************************************
if has("gui_macvim")
  	set fuoptions=maxvert,maxhorz " fullscreen options (MacVim only), resized window when changed to fullscreen
    set guifont=Inconsolata\ for\ Powerline:h15
    set guioptions-=e " don't use gui tab apperance
    set guioptions-=T " hide toolbar
    set guioptions-=r " don't show scrollbars
    set guioptions-=l " don't show scrollbars
    set guioptions-=R " don't show scrollbars
    set guioptions-=L " don't show scrollbars
    set stal=2 " turn on tabs by default
    set gtl=%t gtt=%F " tab headings 
    set background=dark
    colorscheme badwolf
end

" Automatically cd into the directory that the file is in
autocmd BufEnter * execute "chdir ".escape(expand("%:p:h"), ' ')

" Statusline stuff
" Using vim-airline - https://github.com/bling/vim-airline
set laststatus=2
let g:airline_theme='powerlineish'
let g:airline_powerline_fonts=1
" let g:airline_left_sep = ''
" let g:airline_right_sep = ''
" let g:airline_branch_prefix = '⭠'
" let g:airline_readonly_symbol = '⭤'
" let g:airline_linecolumn_prefix = '⭡'
" let g:airline_paste_symbol = 'ρ'
let g:airline_detect_whitespace=0
let g:airline#extensions#tabline#enabled = 1

" NERDTree
map <C-n> :NERDTreeToggle<CR>

" CtrlP settings
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
