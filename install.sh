#!/bin/bash

# Update pacman database and install packages.
sudo pacman -Syu --needed $(comm -12 <(pacman -Slq | sort) <(sort pacman_packages.txt))

# Install yay and aur packages.
cd ~/../../opt && sudo git clone https://aur.archlinux.org/yay.git
sudo chown -R $USER:users ./yay
cd yay
makepkg -si
yay -Syu --needed $(comm -12  <(yay -Slq | sort) <(sort aur_packages.txt))

# Install dmenu.
cd ~/.config
wget https://dl.suckless.org/tools/dmenu-5.2.tar.gz
tar -xf dmenu-*.tar.gz
rm -r dmenu-*.tar.gz
cd dmenu-*
sudo make install 

# Install cursors, icons, and theme.
packages=(
	"WhiteSur-cursors"
	"WhiteSur-icon-theme"
	"WhiteSur-gtk-theme"
)
for package in "${packages[@]}"; do
	cd ~/.dotfiles
	git clone https://github.com/vinceliuice/${package}.git
	cd ${package}
	./install.sh
done

# Create Firefox profile and styling.
FIREFOX_PROFILE=$(ls ~/dotfiles/.mozilla/firefox/)
mkdir -p ~/.mozilla/firefox/$FIREFOX_PROFILE/chrome
wget -O ~/.mozilla/firefox/$FIREFOX_PROFILE/user.js https://raw.githubusercontent.com/arkenfox/user.js/master/user.js
svn --force https://github.com/andreasgrafen/trunk/cascade/chrome ~/.mozilla/firefox/$FIREFOX_PROFILE/chrome

# Create symbolic links to config files.
configs=(
	".bashrc"
	".config/alacritty"
	".config/dmenu/config.h"
	".config/dmenu/dmenu.1"
	".config/dmenu/dmenu.c"
	".config/htop"
	".config/neofetch"
	".config/nitrogen"
	".config/picom"
	".config/xmonad"
	".config/xmobar"
	".mozilla/firefox/$FIREFOX_PROFILE/user-overrides.js"
	".xinitrc"
)
for config in "${configs[@]}"; do
       ln -fsv "${HOME}/.dotfiles/${config}" "${HOME}/${config}"
done

# Use starship's Pure preset config.
starship preset pure-preset -o ~/.config/starship.toml