#!/bin/sh

# Set wallpaper.
nitrogen --restore

# Initialize picom compositor.
picom &

# Restore previous colourscheme with pywal.
wal -R

# Initialize xmonad window manager.
xmonad