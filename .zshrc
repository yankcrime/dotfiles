# .zshrc
# nick@dischord.org

# usual suspects
#
export EDITOR="vim"
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
alias ls='ls -F'
alias tma='tmux attach-session -t'
alias view='vim -R'
alias se='sudoedit'
alias topmem='ps -eo pmem,pcpu,rss,vsize,args | sort -k 1 -r | less'
alias sshx='ssh -c arcfour,blowfish-cbc -XC'
alias pwplz='pwgen -n -y -s 12 1'
alias keyplz='openssl rand -hex 10'
alias md='open -a Marked\ 2.app'
alias uuidgen="uuidgen | tr 'A-Z' 'a-z'"
alias flushdns='sudo dscacheutil -flushcache ; sudo killall -HUP mDNSResponder'
alias docekr='docker'
alias vim='/usr/local/bin/vim'

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
alias gits='git status'
alias gitrs="git reset --soft 'HEAD^'"
alias gitrsh='git reset --hard HEAD'
alias gitsup='git submodule sync ; git submodule update --init'

# emacs
#
alias emacs='open -a /Applications/Emacs.app $1'

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

# prompt
#
# salient bits pilfered from
# https://github.com/gnachman/iterm2-website/tree/master/source/utilities
#
setopt print_exit_value
setopt PROMPT_SUBST

# set the window title
#
precmd() { print -Pn "\e]0;%n@%m:%~\a" }

if [[ $(hostname -s) == deadline ]]; then
    print_osc() {
        if [[ $TERM == tmux* ]] ; then
            printf "\033Ptmux;\033\033]"
        else
            printf "\033]"
        fi
    }
    print_st() {
        if [[ $TERM == tmux* ]] ; then
            printf "\a\033\\"
        else
            printf "\a"
        fi
    }
    git_status() {
        if [ -d .git ]; then
            print_osc
            printf "1337;SetKeyLabel=%s=%s" "status" "🌱 $(git rev-parse --abbrev-ref HEAD)"
            print_st
        else
            print_osc
            printf "1337;SetKeyLabel=%s=%s" "status" "🙈"
            print_st
        fi
    }
    PS1='%{$(git_status)%}%n@%m:%25<..<%~%(!.#.>) '
 else
    PS1='%n@%m:%25<..<%~%(!.#.>) '
fi

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

# change cursor shape based on which vi mode we're in
# via https://emily.st/2013/05/03/zsh-vi-cursor/
#
function zle-keymap-select zle-line-init
{
    case $KEYMAP in
        vicmd)      print -n -- "\E]50;CursorShape=0\C-G";;  # block cursor
        viins|main) print -n -- "\E]50;CursorShape=1\C-G";;  # line cursor
    esac

    zle reset-prompt
    zle -R
}

function zle-line-finish
{
    print -n -- "\E]50;CursorShape=0\C-G"  # block cursor
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

# pyenv and rbenv junk
#
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
if which pyenv > /dev/null; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi

# load zgen and plugins
# https://github.com/tarjoilija/zgen
#
source "${HOME}/.zgen/zgen.zsh"
if ! zgen saved; then
  echo "Creating a zgen save"

  zgen load mafredri/zsh-async
  zgen load junegunn/fzf
  zgen load junegunn/fzf shell/completion.zsh
  zgen load junegunn/fzf shell/key-bindings.zsh
  zgen load rupa/z
  zgen load supercrabtree/k

  zgen save
fi

# Allow use of Ctrl-S in vim
#
stty -ixon

# Last but definitely not least - FZF
#
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# vim:ts=4:sw=4:ft=zsh:et

