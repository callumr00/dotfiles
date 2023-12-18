#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#################################################

# Initalize starship prompt.
eval "$(starship init bash)"

# Set colour scheme with pywal.
(cat ~/.cache/wal/sequences &)

# Modify history settings to ignore duplicates and store 1000 lines.
export HISTCONTROL=ignoredups
export HISTSIZE=1000

# Sync files with storage medium used for backups.
alias backup="sudo rsync -av --delete /home /mnt/backup"

# Serve website at localhost that updates on file changes.
alias serve='browser-sync start --server --files .'

# Recompile CSS file on SCSS file changes.
alias wscss='node-sass -w styles/style.scss styles/style.css'