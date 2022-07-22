#! /bin/bash

ffmpeg -y -video_size 1920x1080 -framerate 30 -f x11grab -i :0.0 $HOME/Videos/screen_recording-$(date +"%F-%R-%N").mp4 &> $HOME/Videos/rec_logs/screen1_recording.log &