# vim:ts=3:et:ft=zsh
# zshell config
# nick@dischord.org

# usual suspects
export PATH=$PATH:~/bin/:/usr/local/bin:/usr/local/sbin:~/.rvm/bin

# history stuff
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.history

# a sensible prompt
PS1='%n@%m:%~%(!.#.>) '

# some other {sensible,useful} shortcuts
alias pg='ps auwwx | grep -i -e ^USER -e '
alias publicip='curl http://ifconfig.me'
alias webserver="ifconfig | grep 'inet ' | grep -v 127.0.0.1; python -m SimpleHTTPServer"
alias cls='clear'
alias ls='ls -FG'
alias tma='tmux attach-session -t'
alias view='vim -R'
alias se='sudoedit'
alias topmem='ps -eo pmem,pcpu,rss,vsize,args | sort -k 1 -r | less'
alias df='df -x rootfs'
alias rip='dvdbackup -i /dev/sr0 -o . -M'
alias rake="noglob rake"

# options
umask 022
setopt PRINT_EXIT_VALUE
TMOUT=0
HISTSIZE=1000
setopt nohup

# set terminal title
precmd () {print -Pn "\e]0;%n@%m: %~\a"}

# stuff that makes zsh worthwhile
autoload -U compinit
compinit

bindkey -v

