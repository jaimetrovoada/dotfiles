#!/bin/bash

# install packages with pacman
function install_packages {

local packages=(
    "dunst",
    "flameshot",
    "polybar",
    "picom",
    "i3-gaps',
    'rofi"
    'neofetch',
    'neovim',
    'zsh',
    'alacritty',
    'ranger',
)

for package in "${packages[@]}"; do
    if ! pacman -Qi $package > /dev/null; then
        echo "Installing $package"
        sudo pacman -S $package --noconfirm
    else
        echo "$package is already installed"
    fi
done

}

# install yay aur helper
function install_yay {
    echo "Installing yay"
    install yay
    pacman -S --needed git base-devel && git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si
    rm -rf yay
    yay -Y --gendb
    yay -Syu --devel
    yay -Y --devel --save
    echo "yay installed"
}

# install SpaceVim neovim config
function install_spacevim {
    echo "Installing SpaceVim"

    echo "SpaceVim installed"
    
}

# isntall oh-my-zsh
function install_omz {
    echo "Installing oh-my-zsh"
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    echo "oh-my-zsh installed"
}

# install nerd fonts
function install_nerdfonts {
    echo "Installing nerd fonts"
    yay -S nerd-fonts-complete
    echo "nerd fonts installed"
}

# install powerline10k
function install_powerline10k {
    echo "Installing powerline10k"
    git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
    echo "powerline10k installed"
}

function base_install {

install_omz
install_powerline10k

# copy files to $HOME
echo "Copying files to $HOME"
echo "Copying .config/"
cp -r .config/ $HOME
echo "Copying .SpaceVim.d/"
cp -r .SpaceVim.d/ $HOME
echo "Copying .zsh/"
cp -r .zsh/ $HOME
echo "Copying .zshrc"
cp -r .zshrc $HOME
echo "Copying .p10k.zsh"
cp -r .p10k.zsh $HOME
}

function arch_install {

install_packages
install_yay
install_nerdfonts
base_install
}

# help function
function show_help {
    echo "Usage: ./setup.sh [OPTION]"
    echo "Options:"
    echo "  -b    Install base dependencies only"
    echo "  -h    Show this help"
}

# Get the options
while getopts ":hb:" option; do
   case $option in
      n) # do base install only
         base_install
         exit;;
      \?) # Invalid option
         show_help
         exit;;
      h) # show help
         show_help
         exit;;
   esac
done


arch_install