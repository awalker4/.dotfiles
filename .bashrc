########################################
# 
# ~/.bashrc
# Start-up settings for Bash
#
########################################

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#Greeting
echo 
echo Hello $USER
echo It is $(date)
echo

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Load CVS variables
if [ -f $HOME/.cvs_env ]; then
    . $HOME/.cvs_env
fi

#Don't save duplicate lines
export HISTCONTROL=erasedups


### #Aliases ####

#Quick and easy telnet
tel () { telnet 192.168.$1.$2 $3; }

# Quick navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

alias h='history'
alias ls='ls -h --color'
alias cp='cp -iv'
alias mv='mv -v'
alias rm='rm -Iv'


#### Functions ####

extract() {
    if [ -f $1 ] ; then
        case $1 in
             *.tar.bz2)   tar xjf $1        ;;
             *.tar.gz)    tar xzf $1     ;;
             *.bz2)       bunzip2 $1       ;;
             *.rar)       rar x $1     ;;
             *.gz)        gunzip $1     ;;
             *.tar)       tar xf $1        ;;
             *.tbz2)      tar xjf $1      ;;
             *.tgz)       tar xzf $1       ;;
             *.zip)       unzip $1     ;;
             *.Z)         uncompress $1  ;;
             *.7z)        7z x $1    ;;
             *)           echo "'$1' cannot be extracted via extract()" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

function repeat()       # Repeat n times command.
{
    local i max
    max=$1; shift;
    for ((i=1; i <= max ; i++)); do
        eval "$@";
    done
}

