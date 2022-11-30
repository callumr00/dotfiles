# Sources dotfiles

# alacritty
cp -r ~/Setup/alacritty ~/Desktop/github/dotfiles

# code
rm -r ~/Desktop/github/dotfiles/code
cp -r ~/Setup/code ~/Desktop/github/dotfiles/code

# dmenu
rm -r dmenu
mkdir dmenu
sudo cp ~/Setup/dmenu/config.h ~/Desktop/github/dotfiles/dmenu
sudo cp ~/Setup/dmenu/dmenu.1 ~/Desktop/github/dotfiles/dmenu
sudo cp ~/Setup/dmenu/dmenu.c ~/Desktop/github/dotfiles/dmenu

# GTK
cp -r ~/Setup/gtk-3.0 ~/Desktop/github/dotfiles

# htop
cp -r ~/Setup/htop ~/Desktop/github/dotfiles

# neofetch
cp -r ~/Setup/neofetch ~/Desktop/github/dotfiles

# picom
cp -r ~/Setup/picom ~/Desktop/github/dotfiles

# wallpapers
rm -r ~/Desktop/github/dotfiles/wallpapers
cp -r ~/Setup/wallpapers ~/Desktop/github/dotfiles/wallpapers

# xmobar
cp -r ~/Setup/xmobar ~/Desktop/github/dotfiles

# xmonad
cp -r ~/Setup/xmonad ~/Desktop/github/dotfiles