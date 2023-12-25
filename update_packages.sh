#!/bin/bash

# Save pacman package list.
pacman -Qqen > ~/.dotfiles/pacman_packages.txt

# Save aur package list.
pacman -Qqem > ~/.dotfiles/aur_packages.txt

# Save npm package list.
npm ls | grep '──' | awk '{print $2}' | awk -F@ '{print $1}' > ~/.dotfiles/npm_packages.txt
