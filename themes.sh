mkdir -p ~/.dotfiles/.themes
cd ~/.dotfiles/.themes

# Install and apply the WhiteSur GTK theme.
GTK_THEME_NAME="WhiteSur-Dark"
GTK_THEME_URL="https://github.com/vinceliuice/WhiteSur-gtk-theme/raw/master/release/WhiteSur-Dark.tar.xz"
wget -q "$GTK_THEME_URL"
tar -xf "$GTK_THEME_NAME".tar.xz
rm "$GTK_THEME_NAME".tar.xz
gsettings set org.gnome.desktop.interface gtk-theme "$GTK_THEME_NAME"

# Install and apply the WhiteSur icon theme.
ICON_THEME_NAME="WhiteSur-Icons"
ICON_THEME_URL="https://github.com/vinceliuice/WhiteSur-icon-theme/trunk/src/"
svn checkout -q "$ICON_THEME_URL" "$ICON_THEME_NAME"
gsettings set org.gnome.desktop.interface icon-theme "$ICON_THEME_NAME"

# Install and apply the McMojave cursor theme.
CURSOR_THEME_NAME="McMojave-Cursors"
CURSOR_THEME_URL="https://github.com/vinceliuice/McMojave-cursors/trunk/dist/"
svn checkout -q "$CURSOR_THEME_URL" "$CURSOR_THEME_NAME"
gsettings set org.gnome.desktop.interface cursor-theme "$CURSOR_THEME_NAME"
