#!/bin/sh

VERSION={{VERSION}}
OUT_EXT={{EXTENSION}}
SOURCE_NAME={{SOURCE}}

function dst_file {
    echo "$SOURCE_NAME.$1.$OUT_EXT"
}
function src_file {
    echo "$SOURCE_NAME.$OUT_EXT"
}

FFMPEG_CMD="ffmpeg "

i=0
for piece in \
{{PIECES}}
;
do IFS=","; set -- $piece
    A=$(echo $1 | cut -d':' -f2)
    if [ $A != "null" ]; then
        strA="-ss $A"
    else
        strA=""
    fi
    B=$(echo $2 | cut -d':' -f2)
    if [ $B != "null" ]; then
        strB="-t $(echo $B-$A | bc)"
    else
        strB=""
    fi
    IFS=" "
    
    printf -v NUMBER_STR '%02d' "$i"
    FFMPEG_CMD="$FFMPEG_CMD $strA $strB -i \"$(src_file)\" -c copy -map $NUMBER_STR \"$(dst_file $NUMBER_STR)\""
    ((i++))
done

echo $FFMPEG_CMD
eval $FFMPEG_CMD
