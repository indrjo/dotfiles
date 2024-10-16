#!/usr/bin/env sh

#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

# Set the commandline prompt.
lambda=$'\u03bb'
PS1="\[\e[1;36m\]\w $lambda\[\e[0m\] "

# Do not include commands starting with a space to the history of commands.
export HISTIGNORE=' *'

# !!! Write on ~/.bash_aliases all your stuff !!!
[ -f ~/.bash_aliases ] && source ~/.bash_aliases

# The defualt editor.
[ "$EDITOR" ] || export EDITOR="vim"

