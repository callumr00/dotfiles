#
# ~/.bashrc
#

# If not running interactively, don't do anything.
[[ $- != *i* ]] && return

#################################################

# Initalize starship prompt.
eval "$(starship init bash)"

# Modify history settings to ignore duplicates and store 1000 lines.
export HISTCONTROL=ignoredups
export HISTSIZE=1000

# Call neofetch with custom ASCII image.
alias status="neofetch --ascii ~/.config/neofetch/logos"

# Print Oura Daily Scores in terminal.
export OURA_DIR=~/oura-daily-scores/
alias oura="(
    python ${OURA_DIR}oura.py &&
    jp2a ${OURA_DIR}daily_scores.png &&
    rm ${OURA_DIR}daily_scores.png
)"

# Sync files with storage medium used for backups.
alias backup="sudo rsync -av --delete /home /mnt/backup"

# Serve website at localhost that updates on file changes.
alias serve='browser-sync start --server --files .'

# Recompile CSS file on SCSS file changes.
alias wscss='node-sass -w styles/style.scss styles/style.css'
