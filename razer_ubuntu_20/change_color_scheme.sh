#!/bin/bash

echo "$1"

if [ "$1" != "light" ] && [ "$1" != "dark" ]; then
	echo "Need to pass \"light\" or \"dark\" as arg"
	exit 1
fi

xdotool mousemove 100 100
xdotool click 3
xdotool mousemove 140 350
xdotool click 1
xdotool mousemove 140 100
sleep 1
xdotool click 1
xdotool mousemove 420 140
xdotool click 1
xdotool mousemove 420 250
xdotool click 1


if [ "$1" = "light" ]; then
	xdotool mousemove 420 230
	xdotool click 1
else
	xdotool mousemove 420 280
	xdotool click 1
fi

xdotool mousemove 840 720
xdotool click 1
