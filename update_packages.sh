#!/bin/bash

# Save pacman package list.
pacman -Qqen > ~/.dotfiles/pacman_packages.txt

# Save aur package list.
pacman -Qqem > ~/.dotfiles/aur_packages.txt
