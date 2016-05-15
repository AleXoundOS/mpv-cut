#!/bin/sh

VERSION="0.1"
OUT_EXT="mkv"
SOURCE_NAME="video_filename"

function dst_file {
    echo "$SOURCE_NAME.$1.$OUT_EXT"
}
function src_file {
    echo "$SOURCE_NAME.$OUT_EXT"
}

FFMPEG_CMD="ffmpeg -i \"$(src_file)\""

i=0
for piece in \
A:0.734533,B:0.834533 \ \
A:1.734533,E:null \
;
do IFS=","; set -- $piece
    A=$(echo $1 | cut -d':' -f2)
    B=$(echo $2 | cut -d':' -f2)
    IFS=" "
    #echo from $A to $B
    printf -v NUMBER_STR '%02d' "$i"
    FFMPEG_CMD="$FFMPEG_CMD -c copy -ss $A -to $B \"$(dst_file $NUMBER_STR)\""
    ((i++))
done

echo $FFMPEG_CMD
eval $FFMPEG_CMD