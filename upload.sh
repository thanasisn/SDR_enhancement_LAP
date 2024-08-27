#!/bin/bash
## created on 2023-02-21

#### enter description here

bwlim=500
rclone="$HOME/PROGRAMS/rclone"
config="$HOME/Documents/rclone.conf"
otheropt=" --checkers=20 --delete-before --stats=30s"
bwlimit="  --bwlimit=${bwlim}k"


if [[ "$(hostname)" = "sagan" ]]; then 
    bwlim=500000
    bwlimit="  --bwlimit=${bwlim}k"
    echo "$(hostname)"
fi
# if [[ "$(hostname)" = "sagan" ]]; then 
#     echo "Upload all pdfs"
#     "${rclone}" ${otheropt} ${bwlimit} --config "$config" --include "*.pdf"  sync "$HOME/MANUSCRIPTS/2022_sdr_trends/" "lapauththanasis:/Trends"
# else
#     echo "This runs only on sagan"
# fi

echo "Upload to lapauththanasis:/Enhance"
"${rclone}" ${otheropt} ${bwlimit} --verbose --config "$config" --max-depth 1 --include "*.{pdf}"                       copy "/home/athan/MANUSCRIPTS/02_enhancement/"        "lapauththanasis:/Enhance"
"${rclone}" ${otheropt} ${bwlimit} --verbose --config "$config" --max-depth 1 --include "*.{pdf,odt,docx}"              copy "/home/athan/MANUSCRIPTS/02_enhancement/article" "lapauththanasis:/Enhance"
"${rclone}" ${otheropt} ${bwlimit} --verbose --config "$config" --max-depth 2 --include "[Aa]rticle*/*.{pdf,odt,docx}"  copy "/home/athan/MANUSCRIPTS/02_enhancement/"        "lapauththanasis:/Enhance"
"${rclone}" ${otheropt} ${bwlimit} --verbose --config "$config" --max-depth 2 --include "SUBMISSION_*/*.{pdf,odt,docx}" copy "/home/athan/MANUSCRIPTS/02_enhancement/"        "lapauththanasis:/Enhance"

#
#"${rclone}" ${otheropt} ${bwlimit} --verbose --config "$config" --max-depth 2 --include "Article*/*.{pdf,odt,docx}"    copy "/home/athan/MANUSCRIPTS/02_enhancement/"        "lapauththanasis:/Enhance"

exit 0 
