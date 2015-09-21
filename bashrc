########################################
# ~/.bashrc
# Bash settings
#
# Sections:
#   -> Start-up
#   -> General
#   -> Prompt
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

# Append to history on every command
export PROMPT_COMMAND='history -a'


########################################
# => Prompt
########################################

function parse_git_branch {

        git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \[\1\]/'

}

function proml {

  local        BLUE="\[\033[0;34m\]"

# OPTIONAL - if you want to use any of these other colors:

  local         RED="\[\033[0;31m\]"

  local   LIGHT_RED="\[\033[1;31m\]"

  local       GREEN="\[\033[0;32m\]"

  local LIGHT_GREEN="\[\033[1;32m\]"

  local       WHITE="\[\033[1;37m\]"

  local  LIGHT_GRAY="\[\033[0;37m\]"

# END OPTIONAL

  local     DEFAULT="\[\033[0m\]"

PS1="\u@\h (\W)$BLUE\$(parse_git_branch) $DEFAULT\$ "

}

proml
