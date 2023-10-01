#!/bin/bash

year=$1

unzip.sh $year
find data/nielsen_extracts/RMS/$year/Movement_Files -type f -name \*.tsv > data/nielsen_extracts/RMS/$year/files.txt
nfiles=$(grep -cve '^\s*$' data/nielsen_extracts/RMS/$year/files.txt)
nchunks=$(echo "scale=0; ($nfiles+5)/6" | bc)
echo "chunks: $nchunks"

clean.sh $year $nchunks