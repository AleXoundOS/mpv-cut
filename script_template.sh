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

FFMPEG_CMD="ffmpeg -i \"$(src_file)\""

i=0
for piece in \
{{PIECES}}
;
do IFS=","; set -- $piece
    A=$(echo $1 | cut -d':' -f2)
    if [ $A != "null" ]; then
        A="-ss $A"
    else
        A=""
    fi
    B=$(echo $2 | cut -d':' -f2)
    if [ $B != "null" ]; then
        B="-to $B"
    else
        B=""
    fi
    IFS=" "
    
    printf -v NUMBER_STR '%02d' "$i"
    FFMPEG_CMD="$FFMPEG_CMD -c copy $A $B \"$(dst_file $NUMBER_STR)\""
    ((i++))
done

echo $FFMPEG_CMD
eval $FFMPEG_CMD
