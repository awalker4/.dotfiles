########################################
# ~/.bashrc
# Bash settings
#
# Sections:
#   -> Start-up
#   -> General
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
if [[ -f ~/.dotfiles/local/bashrc ]]; then
    . ~/.dotfiles/local/bashrc
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

alias ..='cd ..'
