#!/bin/bash

# Save pacman package list.
pacman -Qqen > pacman_packages.txt

# Save aur package list.
pacman -Qqem > aur_packages.txt
