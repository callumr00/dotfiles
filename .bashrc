#
# ~/.bashrc
#

# If not running interactively, don't do anything.
[[ $- != *i* ]] && return

#################################################

export HISTCONTROL=ignoredups
export HISTSIZE=1000

eval "$(starship init bash)"

# Send neofetch output to column for printing.
alias neofetch="neofetch --stdout | column -t -s ':'"

# Serve website at localhost that updates on file changes.
alias serve='browser-sync start --server --files .'

# Recompile CSS file on SCSS file changes.
alias wscss='node-sass -w styles/style.scss styles/style.css'
