" Sourced via .vimrc
" autocmd BufRead /tmp/mutt*      :source ~/.vim/mail
" Refs: http://www.mdlerch.com/emailing-mutt-and-vim-advanced-config.html
" http://wcm1.web.rice.edu/mutt-tips.html

set ft=mail             " needless to say, vim had already guessed that alone :)

setl tw=72	" max line length
setl fo=aw

set comments+=n:\|	" '|' is a quote char.
set comments+=n:%	" '%' as well.

" * <F1> to re-format the current paragraph correctly
" * <F2> to format a line which is too long, and go to the next line
" * <F3> to merge the previous line with the current one, with a correct
"        formatting (sometimes useful associated with <F2>)
"
" These keys might be used both in command mode and edit mode.
"
" <F1> might be smarter to use with the Mail_Del_Empty_Quoted() function
" defined below

nmap	<F1>	gqap
nmap	<F2>	gqqj
nmap	<F3>	kgqj
map!	<F1>	<ESC>gqapi
map!	<F2>	<ESC>gqqji
map!	<F3>	<ESC>kgqji

function! Mail_Del_Empty_Quoted()
  exe "normal :%s/^>[[:space:]\%\|\#>]\\+$//e\<CR>"
endfunction

function! Mail_Begining()
  exe "normal gg"
  if getline (line ('.')) =~ '^From: '
    " if we use edit_headers in Mutt, then go after the headers
    exe "normal /^$\<CR>"
  endif
endfunction

call Mail_Del_Empty_Quoted()
call Mail_Begining()
