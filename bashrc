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
if [[ -f /etc/bashrc ]]; then
    . /etc/bashrc
fi

# Find any local definitions
if [[ -f local/bashrc ]]; then
    . local/bashrc
fi

# Get ls to play nicely with solarized
if [[ -f ~/.dircolors/dircolors.ansi-dark ]]; then
    eval $(dircolors ~/.dircolors/dircolors.ansi-dark)
fi

# Get aliases from file
source ~/.dotfiles/aliases

#Greeting
echo
echo Hello $USER
echo It is $(date)
echo

########################################
# => General
########################################
export TERM=xterm-256color

#Don't save duplicate lines
export HISTCONTROL=erasedups

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
