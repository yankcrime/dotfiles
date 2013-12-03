# nick's .bashrc
# nick@dischord.org

# basics
set -o vi
set show-all-if-ambiguous on
shopt -s checkwinsize
PATH=/usr/local/bin:$PATH:~/bin:~/.rvm/bin:/Applications/VMware\ Fusion.app/Contents/Library:/usr/local/sbin
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
LSCOLORS='Exfxcxdxbxegedabagacad'

export PATH LSCOLORS

# some {sensible,useful} shortcuts
alias pg='ps auwwx | grep -i -e ^USER -e '
alias publicip='curl http://ifconfig.me'
alias webserver="ifconfig | grep 'inet ' | grep -v 127.0.0.1; python -m SimpleHTTPServer"
alias cls='clear'
alias ls='ls -FG'
alias tma='tmux attach-session -t'
alias view='vim -R'
alias trillian='mosh trillian.dischord.org'
alias sshvpn='sshuttle --dns -N -v -r nick@212.13.197.13:53 0/0'
alias mq='msmtp-queue'

# <3 vagrant
alias vup='vagrant up --provider=vmware_fusion'
alias vprov='vagrant provision'
alias vstat='vagrant status'
alias vhalt='vagrant halt'
alias vnuke='vagrant destroy'
alias vssh='vagrant ssh'

# git stuff
alias gitl='git log --pretty=format:"%h - %an, %ar : %s"'
alias gits='git shortlog --numbered --summary'

# set window title for xterm-a-likes
case $TERM in
        xterm* | rxvt* )
                XTITLE="\[\e]0;\u@\h (\w)\a\]" ;;
        * )
                XTITLE="" ;;
esac

# Prompts

# PS1="$XTITLE""┌─[\[\e[34m\]\h\[\e[0m\]][\[\e[32m\]\w\[\e[0m\]]\n└─╼ "
# PS1="\[\033[0;31m\w\] \[\033[0;32m\]$\[\033[0m\] "
# PS1='\[\033[0;31m\]\w \[\033[0;32m\]$\[\033[0m\] '

# Keep it simple...
PS1="$XTITLE"'\u@\h:\w> '

# Set hostname in hardstatus line in tmux
if [ "$TERM" == "screen-256color" ]; then
    function ssh() {
         echo -n -e "\033k$1\033\\"
         /usr/bin/ssh $@
         echo -n -e "\033k`hostname -s`\033\\"
    }

	function mosh() {
		echo -n -e "\033k$1\033\\"
		/usr/local/bin/mosh $@
		echo -n -e "\033k`hostname -s`\033\\"
	}

    # We're on localhost
    echo -e "\033k`hostname -s`\033\\"
fi

# rvm junk
[[ -s "/Users/nick/.rvm/scripts/rvm" ]] && source "/Users/nick/.rvm/scripts/rvm"



