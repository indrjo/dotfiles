#!/usr/bin/bash

#
# Some configuration for Gnome
#

# dark theme
gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"

# install some fonts
sudo pacman -S --needed --noconfirm ttf-cascadia-code
gsettings set org.gnome.desktop.interface monospace-font-name "Cascadia Mono 11"
gsettings set org.gnome.desktop.interface font-name "Ubuntu 11"

# battery percentage
gsettings set org.gnome.desktop.interface show-battery-percentage true

# privacy settings
gsettings set org.gnome.desktop.privacy remove-old-trash-files true
gsettings set org.gnome.desktop.privacy remove-old-temp-files true
gsettings set org.gnome.desktop.privacy old-files-age "uint32 7"
gsettings set org.gnome.desktop.privacy remember-recent-files false
gsettings set org.gnome.desktop.privacy remember-app-usage false

