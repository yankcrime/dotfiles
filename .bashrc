# nick's .bashrc
# nick@dischord.org

set -o vi
set show-all-if-ambiguous on

alias cls='clear'
alias ls='ls -F --color'

alias tma='tmux attach-session -t'

PATH=$PATH:/opt/local/bin:/usr/local/bin:~/bin/:~/.rvm/bin
MANPATH=$MANPATH:/usr/share/man
INFOPATH=$INFOPATH:/opt/local/share/info
LSCOLORS='Exfxcxdxbxegedabagacad'
#USERWM=`which ratpoison`

LESS_TERMCAP_us=$'\e[32m'
LESS_TERMCAP_ue=$'\e[0m'
LESS_TERMCAP_md=$'\e[1;31m'
LESS_TERMCAP_me=$'\e[0m'
LESS=-R
PAGER="less"

export LESS_TERMCAP_us LESS_TERMCAP_ue LESS_TERMCAP_md LESS_TERMCAP_me
export LESS PAGER

export PATH MANPATH INFOPATH LSCOLORS 

case $TERM in
        xterm* | rxvt* | screen )
                XTITLE="\[\e]0;\u@\h (\w)\a\]" ;;
        * )
                XTITLE="" ;;
esac

#PS1="$XTITLE""[\u@\h \W]\\$ "
PS1="$XTITLE""[\u@\h:\w]\n\\$ "

# Set hostname in hardstatus line in screen
if [ "$TERM" == "screen-256color" ]; then
    function ssh() {
         echo -n -e "\033k$1\033\\"
         /usr/bin/ssh $@
          echo -n -e "\033k`hostname -s`\033\\"
    }
    
    function telnet() {
        echo -n -e "\033k$1\033\\"
        /usr/bin/telnet $@
        echo -n -e "\033k`hostname -s`\033\\"
    }
    
    # We're on localhost
    echo -e "\033k`hostname -s`\033\\"
fi


