#!/bin/bash

killall xembedsniproxy
pkill polybar
polybar -r main &
