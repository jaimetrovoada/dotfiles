#!/bin/bash

awk '/^[a-z]/ && last {print "<small>",$0,"\t",last,"</small>"} {last=""} /^#/{last=$0}' $HOME/.xmonad/keybinds |
    column -t -s $'\t' |
    rofi -config $HOME/.config/rofi/config2.rasi -dmenu -i -markup-rows -no-show-icons -width 1000 -lines 15 -yoffset 40