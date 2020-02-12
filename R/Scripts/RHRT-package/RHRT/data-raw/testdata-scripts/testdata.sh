#!/bin/bash

DATE=`date +%Y-%m-%d`
files=(16265 16420 16786 19088)
dir=$1
nsrdbFile="nsrdb.atr.txt"
hrtFile="meanHrt.txt"

[ ! -d "$dir" ] && mkdir "$dir"

# Saving RR-intervals
#for i in ${files[@]};
#	do ann2rr -r nsrdb/$i -a atr -i s3 -w >> "$dir"/"$nsrdbFile";
#done;

# Calculating averaged Interval
Rscript calc_av.R "$dir"/"$nsrdbFile" > "$dir"/"$hrtFile";

# Calculating 
Rscript testdata.R "$dir"/"$hrtFile" $*
