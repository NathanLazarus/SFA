#!/bin/bash

year=$1

# code/unzip.sh $year

tar xzvf "data/SCANNER_DATA_"$year".tgz" -C "data/"
echo "Unzipped Nielsen data for $year"
find data/nielsen_extracts/RMS/$year/Movement_Files -type f -name \*.tsv > data/nielsen_extracts/RMS/$year/files.txt
nfiles=$(grep -cve '^\s*$' data/nielsen_extracts/RMS/$year/files.txt)
nchunks=$(echo "scale=0; ($nfiles+5)/6" | bc)
echo "chunks: $nchunks"
mkdir data/nielsen_extracts/RMS/$year/Clean_Files
mkdir slurm_output/output$year

code/clean.sh $year $nchunks