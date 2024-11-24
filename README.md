# Linux Dotfiles

A minimal installation and configuration for productivity.

![screenshot](/img.png)

OS : [Arch Linux](https://archlinux.org/)\
Window Manager: [xmonad](https://xmonad.org/)\
Status Bar: [xmobar](https://hackage.haskell.org/package/xmobar)\
App Launcher: [rofi](https://davatorium.github.io/rofi/)\
Terminal Emulator: [alacritty](https://github.com/alacritty/alacritty)\
Text Editor: [vim](https://www.vim.org/)

---

<details>
<summary>OS Installation (~15min)</summary>
    
1. Go through the Pre-Installation steps
    1. [Acquire an installation image](https://wiki.archlinux.org/title/installation_guide#Acquire_an_installation_image)
    2. [Verify signature](https://wiki.archlinux.org/title/installation_guide#Verify_signature)
    3. [Prepare an installation medium](https://wiki.archlinux.org/title/installation_guide#Prepare_an_installation_medium)
    4. [Boot the live environment](https://wiki.archlinux.org/title/installation_guide#Boot_the_live_environment)
  
2. Use the [archinstall](https://wiki.archlinux.org/title/archinstall) helper
    1. `archinstall`
    2. 
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
| Host name                 | *Host name*                                                   |
| Root password             | *Root password*                                               |
| User account              | *User account*                                                |
| Profile/Type              | Minimal                                                       |
| Audio                     | Pipewire                                                      |
| Kernels                   | linux                                                         |
| Additional Packages       | git                                                           |
| Network configuration     | Use NetworkManager                                            |
| Timezone                  | Europe/London                                                 |
| Automatic time sync (NTP) | True                                                          |
| Optional Repositories     | multilib                                                      |
    
3. `reboot` 
4. Enter the BIOS, boot the chosen harddrive, and login.

</details>

<details>
<summary>Customization (~45min)</summary>
<br>
    
1. Clone this repo
```
git clone https://github.com/callumr00/dotfiles.git ~/.dotfiles
```
2. Run the install script

The installation script installs packages using <a href="https://wiki.archlinux.org/title/pacman">pacman</a>, the Arch Linux package manager; it won't work out-of-the-box for other distros. Symlinks are then created for config files.

```
~/.dotfiles/install.sh
```
3. `reboot`
4. Login and enter the desktop environment with `startx`
</details>
