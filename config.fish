# User customisation for the Fish shell

# vi related
set fish_key_bindings fish_vi_key_bindings
function fish_mode_prompt; end

# better compatibility for fzf with tmux
set -U FZF_TMUX 1

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
# alias rake="noglob rake"
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

set EDITOR "nvim"
set HOMEBREW_GITHUB_API_TOKEN ""
set GOPATH ~/src/golang
set KEYTIMEOUT 1

export EDITOR HOMEBREW_GITHUB_API_TOKEN GOPATH KEYTIMEOUT
