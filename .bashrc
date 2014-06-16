########################################
# ~/.bashrc
# Bash settings
#
# Sections:
#   -> Start-up 
#   -> General
#   -> Aliases, shortcuts
#   -> Functions
#   -> Work stuff
########################################


########################################
# => Start-up
########################################

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Load CVS variables
if [ -f $HOME/.cvs_env ]; then
    . $HOME/.cvs_env
fi

#Greeting
echo 
echo Hello $USER
echo It is $(date)
echo


########################################
# => General
########################################

#Don't save duplicate lines
export HISTCONTROL=erasedups


########################################
# => Aliases, shortcuts
########################################

#Some straightforward ones
alias ls='ls -h --color'
alias cp='cp -iv'
alias mv='mv -v'
alias rm='rm -Iv'
alias h='history'


#Quick and easy telnet
tel () { telnet 192.168.$1.$2 $3; }

#Use (..)^n to cd up n times
function .. () {
  local arg=${1:-1};
  while [ $arg -gt 0 ]; do
    cd .. >&/dev/null;
    arg=$(($arg - 1));
  done
}


########################################
# => Functions
########################################

# Generic extracting
extract() {
    if [ -f $1 ] ; then
        case $1 in
             *.tar.bz2)   tar xjf $1;;
             *.tar.gz)    tar xzf $1;;
             *.bz2)       bunzip2 $1;;
             *.rar)       rar x $1;;
             *.gz)        gunzip $1;;
             *.tar)       tar xf $1;;
             *.tbz2)      tar xjf $1;;
             *.tgz)       tar xzf $1;;
             *.zip)       unzip $1;;
             *.Z)         uncompress $1;;
             *.7z)        7z x $1;;
             *)           echo "'$1' cannot be extracted via extract()";;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

# 'repeat i cmd' will do cmd i times
repeat() {
    local i max
    max=$1; shift;
    for ((i=1; i <= max ; i++)); do
        eval "$@";
    done
}


########################################
# => Work stuff
########################################

# Build tools are here
export PATH=$PATH:/users/tools/bin/

# Private mockbuild
alias mock="/usr/bin/mock --configdir=/users/awalker/mockbuild/cfg"
