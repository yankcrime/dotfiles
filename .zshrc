# .zshrc
# nick@dischord.org

# don't do anything if its emacs tramp mode
#
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# set theme based on OS preference
# do this early otherwise there's a noticable change
#
if [[ "$(uname -s)" == "Darwin" ]]; then
    darkmode() {
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

    darkmode
fi

# usual suspects
#
export EDITOR="nvim"
export GPG_TTY=$(tty)
export BAT_THEME="ansi"
export FZF_DEFAULT_OPTS="--color=bw"
export KEYTIMEOUT=1

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

autoload -Uz +X compaudit compinit
autoload -Uz +X bashcompinit

# only bother with rebuilding, auditing, and compiling the compinit
# file once a whole day has passed. The -C flag bypasses both the
# check for rebuilding the dump file and the usual call to compaudit.
# via @emilyst
#
setopt EXTENDEDGLOB
for dump in $HOME/.zcompdump(N.mh+24); do
  echo 'Re-initializing ZSH completions'
  touch $dump
  compinit
  bashcompinit
  if [[ -s "$dump" && (! -s "$dump.zwc" || "$dump" -nt "$dump.zwc") ]]; then
    zcompile "$dump"
  fi
done
unsetopt EXTENDEDGLOB
compinit -C

# other stuff that makes zsh worthwhile
#
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
#if [[ $(hostname -s) == deadline ]]; then
#  alias docker='lima nerdctl'
#fi
alias docekr='docker'
alias vim='nvim'
alias k='kubectl'
alias klogs='kubectl logs'
alias rdecs='xprop -f _MOTIF_WM_HINTS 32c -set _MOTIF_WM_HINTS "0x2, 0x0, 0x0, 0x0, 0x0"'
###source <(kubectl completion zsh)
source ~/.kubectl_completion
alias lgc='sudo chown nick:kvm /dev/shm/looking-glass ; chmod 660 /dev/shm/looking-glass ; looking-glass-client -F'
alias mp='multipass'
alias tfapply="terraform apply plan.out"
alias tfplan="terraform plan -out plan.out"
alias tfdestroy="terraform destroy -force"
alias k9s='k9s --headless'
alias kchere='export KUBECONFIG=$(pwd)/kubeconfig.yaml'

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
    RPROMPT='%F{#aaaaaa}%D{%H:%M:%S}%f%F{#000000}%f'
    RPROMPT="%D{%H:%M:%S}"
}

# krew
#
export PATH="${PATH}:${HOME}/.krew/bin"

# load zgen and plugins
# https://github.com/tarjoilija/zgen
#
source "${HOME}/.zgen/zgen.zsh"
if ! zgen saved; then
  echo "Creating a zgen save"

  zgen load jeffreytse/zsh-vi-mode
  zgen load junegunn/fzf
  zgen load junegunn/fzf shell/completion.zsh
  zgen load junegunn/fzf shell/key-bindings.zsh
  zgen load agkozak/zsh-z
  zgen load mroth/evalcache

  zgen save
fi

# Allow use of Ctrl-S in vim
#
stty -ixon

# Fuzzy history search via fzf
#
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Assume we always want insert mode
#
ZVM_LINE_INIT_MODE=$ZVM_MODE_INSERT

# Reinstate ctrl-r to use FZF
#
function zvm_after_init() {
  zvm_bindkey viins '^R' fzf-history-widget
}

# init junk via evalcache
#
#_evalcache starship init zsh
_evalcache pyenv init -
_evalcache rbenv init -

# vim:ts=4:sw=4:ft=zsh:et

