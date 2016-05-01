#!/bin/sh

VERSION=0.1
OUT_EXT="mkv"
SOURCE_NAME=""

function dst_file {
    echo "$SOURCE_NAME.$1.$OUT_EXT"
}
function src_file {
    echo "$SOURCE_NAME.$OUT_EXT"
}

FFMPEG_CMD="ffmpeg -i \"$(src_file)\""

i=0
for piece in \
A:0.0,B:3.0 \
A:1.5,X:7.5 \
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
