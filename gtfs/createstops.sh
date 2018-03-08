#!/bin/sh
#concatenates stop id file and creates a scv with stop_id and stop_name columns
echo "Name for an output file?"
read OUTFILE 
(head -1 stopgtfsbx.csv ; tail -n +2 -q stopgtfs*.csv) | \
awk -F, '{print $1","$2}' | \
sort | \
awk -F, '{ if (a[$1]++ == 0) print $0; }' "$@" \
> $OUTFILE

