# dotfiles

A minimal installation and configuration for productivity.

![screenshot](/screenshot.jpg)

Window Manager: [xmonad](https://xmonad.org/)\
Status Bar: [xmobar](https://hackage.haskell.org/package/xmobar)\
App Launcher: [rofi](https://davatorium.github.io/rofi/)\
Terminal Emulator: [alacritty](https://github.com/alacritty/alacritty)\
Terminal Prompt: [starship](https://starship.rs/) (with the [Pure](https://starship.rs/presets/pure-preset.html) preset)\
Text Editor: [vim](https://www.vim.org/)

---

<details>
<summary>OS Installation (~15min)</summary>
<br>

Go through the Pre-Installation steps:
1. [Acquire an installation image](https://wiki.archlinux.org/title/installation_guide#Pre-installation)
2. [Verify signature](https://wiki.archlinux.org/title/installation_guide#Prepare_an_installation_medium)
3. [Prepare an installation medium](https://wiki.archlinux.org/title/installation_guide#Prepare_an_installation_medium)
4. [Boot the live environment](https://wiki.archlinux.org/title/installation_guide#Prepare_an_installation_medium)

Use the [archinstall](https://wiki.archlinux.org/title/archinstall) helper:

5. ```archinstall``` when prompted
6. Configure the installation:

| Field                     | Selection                                                     |
|---------------------------|---------------------------------------------------------------|
| Archinstall Language      | English (100%)                                                |
| Mirrors/Mirror region     | United Kingdom                                                |
| Locales/Keyboard layout   | us                                                            |
| Locales/Locale language   | en_US                                                         |
| Locales/Locale encoding   | UTF-8                                                         |
| Disk configuration        | Use a best-effort default partition layout → *disk* → ext4    |
| Disk encryption           | *Encryption password*                                         |
| Bootloader                | Grub                                                          |
| Swap                      | True                                                          |
| Host name                 | *host name*                                                   |
| Root password             | *root password*                                               |
| User account              | *user account*                                                |
| Profile                   | Minimal                                                       |
| Audio                     | Pipewire                                                      |
| Kernels                   | linux                                                         |
| Additional Packages       | git                                                           |
| Network configuration     | Use NetworkManager                                            |
| Timezone                  | Europe/London                                                 |
| Automatic time sync (NTP) | True                                                          |
| Optional Repositories     | multilib                                                      |

7. `reboot` when finished.
8. Enter the BIOS and boot the chosen harddrive, then login.
</details>

<details>
<summary>Customization (~45min)</summary>
<br>

1. Clone this repo
```
git clone https://github.com/callumr00/dotfiles.git ~/.dotfiles
```
2. Run the install script
```
~/.dotfiles/install.sh
```
3. `reboot` and login 
4. Enter the desktop environment with `startx`
</details>
