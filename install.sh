#!/bin/bash

# Install pacman packages.
if [ -f ~/.dotfiles/packages/pacman.txt ]; then
	sudo pacman -Syu --needed $(comm -12 <(pacman -Slq | sort) <(sort ~/.dotfiles/packages/pacman.txt))
fi

# Install yay and aur packages.
if [ -f ~/.dotfiles/packages/aur.txt ]; then
	sudo pacman -Sy --needed git base-devel
	git clone https://aur.archlinux.org/yay.git ~/yay
	cd ~/yay
	makepkg -si
	cd
	yay -Syu --needed $(comm -12  <(yay -Slq | sort) <(sort ~/.dotfiles/packages/aur.txt))
fi

# Install npm and npm packages.
if [ -f ~/.dotfiles/packages/npm.txt ]; then
	pacman -Sy --needed npm
	sudo npm install -g $(tr '\n' ' ' < ~/.dotfiles/packages/npm.txt)
fi

# Create symlinks to config files.
configs=(
	".bashrc"
    ".vimrc"
	".xinitrc"
	".config/alacritty"
	".config/neofetch"
    ".config/rofi"
    ".config/xmobar"
	".config/xmonad"
)
for config in "${configs[@]}"; do
       ln -fsv "${HOME}/.dotfiles/${config}" "${HOME}/${config}"
done
