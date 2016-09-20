# .zshrc
# nick@dischord.org

# usual suspects
#
export PATH=~/.rbenv/bin:$PATH:~/bin:/usr/local/bin:/usr/local/sbin:
# export LSCOLORS="exfxcxdxbxegedabagacad"
export EDITOR="vim"
export HOMEBREW_GITHUB_API_TOKEN=""
export GOPATH=~/src/golang
export VAGRANT_DEFAULT_PROVIDER="vmware_fusion"
export PURE_PROMPT_SYMBOL="$"

# history and general options
#
HISTSIZE=10000000
SAVEHIST=10000000
TMOUT=0
HISTFILE=~/.history
setopt APPEND_HISTORY
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt nohup
umask 022

# some other {sensible,useful} shortcuts
#
alias pg='ps auwwx | grep -i -e ^USER -e '
alias publicip='curl http://ifconfig.me'
alias webserver="ifconfig | grep 'inet ' | grep -v 127.0.0.1; python -m SimpleHTTPServer"
alias cls='clear'
alias ls='ls -FG'
alias tma='tmux attach-session -t'
alias view='vim -R'
alias se='sudoedit'
alias topmem='ps -eo pmem,pcpu,rss,vsize,args | sort -k 1 -r | less'
alias rip='dvdbackup -i /dev/sr0 -o . -M'
# alias rake="noglob rake"
alias trillian='mosh trillian.dischord.org'
alias zarquon='mosh zarquon.dischord.org'
alias sshx='ssh -c arcfour,blowfish-cbc -XC'
alias pwplz='apg -n 1 -m 12 -x 12 -M NC'
alias keyplz='openssl rand -hex 10'
alias md='open -a Marked.app'
alias uuidgen="uuidgen | tr 'A-Z' 'a-z'"
alias mutt='cd ~/Desktop && mutt'
alias flushdns='sudo dscacheutil -flushcache ; sudo killall -HUP mDNSResponder'
alias docekr='docker'
# alias vim='nvim'
alias papply='sudo puppet apply --modulepath /etc/dischord/modules --hiera_config /etc/dischord/hiera.yaml --manifestdir /etc/dischord/ /etc/dischord/default.pp'

# <3 vagrant
#
alias vup='vagrant up --no-install-provider'
alias vprov='vagrant provision'
alias vstat='vagrant status'
alias vhalt='vagrant halt'
alias vnuke='vagrant destroy -f'
alias vssh='vagrant ssh'
alias vhosts='vagrant hostmanager --provider=vmware_fusion'

# git stuff
#
alias gitl='git log --pretty=format:"%h - %an, %ar : %s"'
alias gits='git shortlog --numbered --summary'
alias gitrs="git reset --soft 'HEAD^'"
alias gitrsh='git reset --hard HEAD'
alias gitsup='git submodule sync ; git submodule update --init'


# stuff that makes zsh worthwhile
#
autoload -U compinit && compinit
autoload -U promptinit && promptinit
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
local knownhosts
knownhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*:(ssh|scp|sftp):*' hosts $knownhosts

# make it work like vim
# thanks dougblack - http://dougblack.io/words/zsh-vi-mode.html
#
bindkey -v
bindkey '^P' up-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-pattern-search-backward
export KEYTIMEOUT=1

# pyenv and rbenv junk
#
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

# added by travis gem
#
[ -f /Users/nick/.travis/travis.sh ] && source /Users/nick/.travis/travis.sh

# load zgen and plugins
# https://github.com/tarjoilija/zgen
#
source "${HOME}/.zgen/zgen.zsh"
if ! zgen saved; then
  echo "Creating a zgen save"

  zgen load mafredri/zsh-async
  zgen load sindresorhus/pure
  zgen load junegunn/fzf
  zgen load junegunn/fzf shell/completion.zsh
  zgen load junegunn/fzf shell/key-bindings.zsh
  zgen load rupa/z
  zgen load felixr/docker-zsh-completion

  zgen save
fi

# vim:ts=4:sw=4:ft=zsh:et
