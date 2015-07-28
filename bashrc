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

########################################
# => General
########################################
export TERM=xterm-256color
export EDITOR=vim
export LEDGER_FILE="$HOME/Dropbox/ledger.dat"
export PATH=$PATH:$HOME/bin:/usr/local/bin:$HOME/.gem/ruby/2.2.0/bin:/home/austin/.cabal/bin/
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk/jre

#Don't save duplicate lines
export HISTCONTROL=erasedups

shopt -s autocd

# Context search through history (like zsj)
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

bind 'set completion-ignore-case on'
