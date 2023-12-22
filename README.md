# dotfiles:q

A minimal installation and configuration for productivity.

![wallpaper](/img/wallpapers/logos.png)
![screenshot](/img/screenshot.png)

Window Manager → [xmonad](https://xmonad.org/)\
Menu → [dmenu](https://tools.suckless.org/dmenu/)\
Terminal Emulator → [alacritty](https://github.com/alacritty/alacritty)\
Terminal Prompt → [starship](https://starship.rs/) (with [Pure](https://starship.rs/presets/pure-preset.html) preset)\
Text Editor → [vim](https://www.vim.org/)
    
GTK Theme → [WhiteSur GTK Theme](https://github.com/vinceliuice/WhiteSur-gtk-theme)\
Icon Theme → [WhiteSur Icon Theme](https://github.com/vinceliuice/WhiteSur-icon-theme)\
Cursor Theme → [McMojave Cursors](https://github.com/vinceliuice/McMojave-cursors)

Browser → [firefox](https://www.mozilla.org/en-GB/firefox/)\
Profile → [arkenfox](https://github.com/arkenfox/user.js)\
Addons → [uBlock Origin](https://addons.mozilla.org/en-GB/firefox/addon/ublock-origin/), [Skip Redirect](https://addons.mozilla.org/en-GB/firefox/addon/skip-redirect/), [Proton Pass](https://addons.mozilla.org/en-GB/firefox/addon/proton-pass/)

---

<details>
<summary>OS Installation (~15min)</summary>
<br>

First, go through the Pre-Installation steps below.
1. [Acquire an installation image](https://wiki.archlinux.org/title/installation_guide#Pre-installation)
2. [Verify signature](https://wiki.archlinux.org/title/installation_guide#Prepare_an_installation_medium)
3. [Prepare an installation medium](https://wiki.archlinux.org/title/installation_guide#Prepare_an_installation_medium)
4. [Boot the live environment](https://wiki.archlinux.org/title/installation_guide#Prepare_an_installation_medium)

Next, use the [archinstall](https://wiki.archlinux.org/title/archinstall) helper.

5. ```archinstall``` when prompted
6. Fill the fields when prompted, I have included what I use as a template below:

| Field                     | Selection                                     |
|---------------------------|-----------------------------------------------|
| Archinstall Language      | English (100%)                                |
| Keyboard Layout           | us                                            |
| Mirror Region             | United Kingdom                                |
| Locale Language           | en_US                                         |
| Locale Encoding           | UTF-8                                         |
| Drive(s)                  | *Your Disks*                                  |
| Disk Layout               | wipe all &rarr; ext4 &rarr; no                |
| Encryption Password       | *None*                                        |
| Bootloader                | systemd-bootctl                               |
| Swap                      | True                                          |
| Host name                 | *Your Host Name*                              |
| Root password             | *Your Root Password*                          |
| User account              | *Your Accounts(s)*                            |
| Profile                   | minimal                                       |
| Audio                     | pipewire                                      |
| Kernels                   | linux                                         |
| Additional Packages       | *None*                                        |
| Network configuration     | use NetworkManager                            |
| Timezone                  | Europe/London                                 |
| Automatic time sync (NTP) | True                                          |
| Optional Repositories     | *None*                                        |

7. ```reboot``` when finished.
8. Enter the BIOS and boot the chosen harddrive, then login.
</details>

<details>
<summary>Customization (~45min)</summary>
<br>

1. Install git: `sudo pacman -S git`
2. Clone this repo: `git clone https://github.com/callumr00/dotfiles.git --branch minimal ~/.dotfiles`
3. Run the install script: `~/.dotfiles/install.sh`
4. Reboot: `reboot`

By default, upon logging in you will remain at the command line. To enter the desktop environment, run `startx`.
</details>
