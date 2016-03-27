# vim:ts=3:et:ft=zsh
# .zshrc
# nick@dischord.org

# usual suspects
export PATH=~/.rbenv/bin:$PATH:~/bin:/usr/local/bin:/usr/local/sbin:
# export LSCOLORS="exfxcxdxbxegedabagacad"
export EDITOR="vim"
export HOMEBREW_GITHUB_API_TOKEN=""
export GOPATH=~/src/golang/work
# export NVIM_TUI_ENABLE_TRUE_COLOR=1

# history stuff
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.history
setopt APPEND_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt EXTENDED_HISTORY

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
alias rip='dvdbackup -i /dev/sr0 -o . -M'
alias rake="noglob rake"
alias trillian='mosh trillian.dischord.org'
alias zarquon='mosh zarquon.dischord.org'
alias sshx='ssh -c arcfour,blowfish-cbc -XC'
alias pwplz='apg -n 1 -m 12 -x 12 -M NC'
alias keyplz='openssl rand -hex 10'
alias md='open -a Marked.app'
alias uuidgen="uuidgen | tr 'A-Z' 'a-z'"

# <3 vagrant
alias vup='vagrant up --no-install-provider'
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
alias gitsup='git submodule sync ; git submodule update --init'

# options
umask 022
#setopt PRINT_EXIT_VALUE
TMOUT=0
HISTSIZE=1000
setopt nohup

# stuff that makes zsh worthwhile
autoload -U compinit && compinit
autoload -U promptinit && promptinit
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# a decent prompt
# https://github.com/sindresorhus/pure
prompt pure
unsetopt prompt_cr

# make it work like vim
# thanks dougblack - http://dougblack.io/words/zsh-vi-mode.html
bindkey -v
bindkey '^P' up-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-pattern-search-backward
export KEYTIMEOUT=1
# end vim guff

# jumparound
source ~/bin/z.sh

# ssh wrapper that rename current tmux window to the hostname of the
# remote host.
ssh() {
    # Do nothing if we are not inside tmux or ssh is called without arguments
    if [[ $# == 0 || -z $TMUX ]]; then
        command ssh $@
        return
    fi
    # The hostname is the last parameter (i.e. ${(P)#})
    local remote=${${(P)#}%.*}
    local old_name="$(tmux display-message -p '#W')"
    local renamed=0
    # Save the current name
    if [[ $remote != -* ]]; then
        renamed=1
        tmux rename-window $remote
    fi
    command ssh $@
    if [[ $renamed == 1 ]]; then
        tmux rename-window "$old_name"
    fi
}

# pyenv and rbenv junk
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

# syntax highlighting
source ~/src/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_STYLES[globbing]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[path]='bold'

# added by travis gem
[ -f /Users/nick/.travis/travis.sh ] && source /Users/nick/.travis/travis.sh
