#!/bin/bash

# Update pacman database and install packages.
sudo pacman -Syu --needed $(comm -12 <(pacman -Slq | sort) <(sort ~/.dotfiles/pacman_packages.txt))

# Install yay and aur packages.
cd ~/../../opt && sudo git clone https://aur.archlinux.org/yay.git
sudo chown -R $USER:users ./yay
cd yay
makepkg -si
yay -Syu --needed $(comm -12  <(yay -Slq | sort) <(sort ~/.dotfiles/aur_packages.txt))

# Install themes.
mkdir -p ~/.themes
cd ~/.themes

# -- Install and apply the WhiteSur GTK theme.
GTK_THEME_NAME="WhiteSur-Dark"
GTK_THEME_URL="https://github.com/vinceliuice/WhiteSur-gtk-theme/raw/master/release/WhiteSur-Dark.tar.xz"
wget -q "$GTK_THEME_URL"
tar -xf "$GTK_THEME_NAME".tar.xz
rm "$GTK_THEME_NAME".tar.xz
gsettings set org.gnome.desktop.interface gtk-theme "$GTK_THEME_NAME"

# -- Install and apply the WhiteSur icon theme.
ICON_THEME_NAME="WhiteSur-Icons"
ICON_THEME_URL="https://github.com/vinceliuice/WhiteSur-icon-theme/trunk/src/"
svn checkout -q "$ICON_THEME_URL" "$ICON_THEME_NAME"
gsettings set org.gnome.desktop.interface icon-theme "$ICON_THEME_NAME"

# -- Install and apply the McMojave cursor theme.
CURSOR_THEME_NAME="McMojave-Cursors"
CURSOR_THEME_URL="https://github.com/vinceliuice/McMojave-cursors/trunk/dist/"
svn checkout -q "$CURSOR_THEME_URL" "$CURSOR_THEME_NAME"
gsettings set org.gnome.desktop.interface cursor-theme "$CURSOR_THEME_NAME"

# Create Firefox profile and styling.
FIREFOX_PROFILE=$(ls ~/dotfiles/.mozilla/firefox/)
mkdir -p ~/.mozilla/firefox/$FIREFOX_PROFILE/chrome
wget -O ~/.mozilla/firefox/$FIREFOX_PROFILE/user.js https://raw.githubusercontent.com/arkenfox/user.js/master/user.js
svn --force https://github.com/andreasgrafen/trunk/cascade/chrome ~/.mozilla/firefox/$FIREFOX_PROFILE/chrome

# Install the onedark.vim colour scheme for vim.
mkdir -p ~/.vim/pack/github/start
cd ~/.vim/pack/github/start
git clone https://github.com/joshdick/onedark.vim.git

# Create symbolic links to config files.
configs=(
	".bashrc"
	".config/alacritty"
	".config/neofetch"
    ".config/nitrogen"
    ".config/xmobar"
	".config/xmonad"
	".mozilla/firefox/$FIREFOX_PROFILE/user-overrides.js"
    ".vim"
    ".vimrc"
	".xinitrc"
)
for config in "${configs[@]}"; do
       ln -fsv "${HOME}/.dotfiles/${config}" "${HOME}/${config}"
done

# Use starship's Pure preset config.
starship preset pure-preset -o ~/.config/starship.toml
