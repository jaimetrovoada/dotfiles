#! /bin/bash

if [ $XDG_SESSION_TYPE = "x11" ]
then
    rofi -show drun
else 
    rofi -config $HOME/.config/rofi/config-wayland.rasi -show drun
fi