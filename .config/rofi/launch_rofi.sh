#! /bin/bash

if [ $XDG_SESSION_TYPE = "x11" ]
then
    rofi -show drun
else 
    #tofi-drun -c $HOME/.config/tofi/config
  rofi -config $HOME/.config/rofi/config-wayland.rasi -show drun
fi
