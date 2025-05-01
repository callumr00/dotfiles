#!/usr/bin/env bash

error_prefix="[\033[31;1m0\033[0m]"
success_prefix="[\033[32;1m1\033[0m]"

function install_packages() {
    local package_list=$1

    local installed_packages=$(pacman -Slq | sort)
    local required_packages=$(sort "${package_list}")

    local outstanding_packages=$(comm -12 <(echo "${installed_packages}") \
                                          <(echo "${required_packages}"))

    if [ -n "${outstanding_packages}" ]; then
        sudo pacman -Syu --needed ${outstanding_packages}
    fi
}

function create_symlinks() {
    local dotfiles_dir=$1

    shift
    local configs=("$@")

    for config in "${configs[@]}"; do
        ln -fsv "${dotfiles_dir}/${config}" ${HOME}/${config}
    done
}

function main() {
    local dotfiles_dir="${HOME}/.dotfiles"

    if [ ! -d "${dotfiles_dir}" ]; then
        echo -e "${error_prefix} ${dotfiles_dir} not found"
        return 1
    fi

    local package_list="${dotfiles_dir}/packages.txt"

    if [ ! -f "${package_list}" ]; then
        echo -e "${error_prefix} ${package_list} not found"
        return 1
    fi

    install_packages "${package_list}"

    local configs=(
        ".bashrc"
        ".vimrc"
        ".xinitrc"
        ".config/alacritty"
        ".config/rofi"
        ".config/xmobar"
        ".config/xmonad"
    )

    create_symlinks "${dotfiles_dir}" "${configs[@]}"
}

main
