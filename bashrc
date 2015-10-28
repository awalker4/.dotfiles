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

# Get aliases, variables from files
source ~/.dotfiles/variables
source ~/.dotfiles/aliases

########################################
# => General
########################################
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
