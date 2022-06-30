# .zshrc
# nick@dischord.org

# don't do anything if its emacs tramp mode
#
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# set theme based on OS preference
# do this early otherwise there's a noticable change
#
if [[ "$(uname -s)" == "Darwin" ]]; then
    sith() {
        val=$(defaults read -g AppleInterfaceStyle 2>/dev/null)
        if [[ $val == "Dark" ]]; then
            i
        fi
    }

    i() {
        if [[ $ITERM_PROFILE == "Default" ]]; then
            echo -ne "\033]50;SetProfile=Dark\a"
            export ITERM_PROFILE="Dark"
        else
            echo -ne "\033]50;SetProfile=Terminal\a"
            export ITERM_PROFILE="Default"
        fi
    }

    sith
fi

# usual suspects
#
export EDITOR="nvim"
export GPG_TTY=$(tty)
export BAT_THEME="ansi"

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

# some other {sensible,useful} shortcuts
#
alias pg='ps auwwx | grep -i -e ^USER -e '
alias publicip='curl http://icanhazip.com'
alias webserver="ifconfig | grep 'inet ' | grep -v 127.0.0.1; python -m SimpleHTTPServer"
alias cls='clear'
alias ls='ls -F'
alias tma='tmux attach-session -t'
alias view='vim -R'
alias se='sudoedit'
alias topmem='ps -eo pmem,pcpu,rss,vsize,args | sort -k 1 -r | less'
alias sshx='ssh -c arcfour,blowfish-cbc -XC'
alias pwplz='pwgen -n -y -s 18 1'
alias keyplz='openssl rand -hex 10'
alias md='open -a Marked\ 2.app'
alias uuidgen="uuidgen | tr 'A-Z' 'a-z'"
alias flushdns='sudo dscacheutil -flushcache ; sudo killall -HUP mDNSResponder'
if [[ $(hostname -s) == ignition ]]; then
  alias docker='lima nerdctl'
fi
alias docekr='docker'
alias vim='nvim'
alias k='kubectl'
alias rdecs='xprop -f _MOTIF_WM_HINTS 32c -set _MOTIF_WM_HINTS "0x2, 0x0, 0x0, 0x0, 0x0"'
source <(kubectl completion zsh)
alias lgc='sudo chown nick:kvm /dev/shm/looking-glass ; chmod 660 /dev/shm/looking-glass ; looking-glass-client -F'
alias mp='multipass'
alias tfapply="terraform apply plan.out"
alias tfplan="terraform plan -out plan.out"
alias tfdestroy="terraform destroy -force"
alias k9s='k9s --headless'
alias kchere='export KUBECONFIG=$(pwd)/kube_config_cluster.yml'

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
alias emacs='emacsclient -n'

# prompt and window title
#
setopt print_exit_value
setopt PROMPT_SUBST

autoload -Uz vcs_info
zstyle ':vcs_info:git:*' formats 'on %b '

precmd() {
    vcs_info
    print -Pn "\e]0;%n@%m:%~\a"
    PROMPT='%n@%m %25<..<%~%(!.#. ${vcs_info_msg_0_}%%) %(1j.%F{green}·%j%f .)'
}

# make it work like vim
#
bindkey -v
bindkey '^P' up-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-pattern-search-backward
export KEYTIMEOUT=1

bindkey '^[[Z' reverse-menu-complete # make shift-tab work in reverse

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
#if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
#
eval "$(pyenv init -)"

# krew
#
export PATH="${PATH}:${HOME}/.krew/bin"

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
  zgen load agkozak/zsh-z
  zgen load ~/src/k

  zgen save
fi

# Allow use of Ctrl-S in vim
#
stty -ixon

# Fuzzy history search via fzf
#
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# vim:ts=4:sw=4:ft=zsh:et
