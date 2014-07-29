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
alias trillian='mosh trillian.dischord.org'
alias zarquon='mosh zarquon.dischord.org'
alias sshx='ssh -c arcfour,blowfish-cbc -XC'
alias pwplz='apg -n 1 -m 12 -x 12 -M NC'
alias vim='mvim -v'

# <3 vagrant
alias vup='vagrant up --provider=vmware_fusion'
alias vprov='vagrant provision'
alias vstat='vagrant status'
alias vhalt='vagrant halt'
alias vnuke='vagrant destroy -f'
alias vssh='vagrant ssh'
alias vhosts='vagrant hostmanager --provider=vmware_fusion'

# git stuff
alias gitl='git log --pretty=format:"%h - %an, %ar : %s"'
alias gits='git shortlog --numbered --summary'
alias gitrs="git reset --soft 'HEAD^'"
alias gitrsh='git reset --hard HEAD'
alias gitsup='for foo in init sync update ; do git submodule $foo ; done'

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

# make it work like vim
# thanks dougblack - http://dougblack.io/words/zsh-vi-mode.html
bindkey -v

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward

function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select
export KEYTIMEOUT=1

source ~/src/opp.zsh/opp.zsh
# end vim guff
