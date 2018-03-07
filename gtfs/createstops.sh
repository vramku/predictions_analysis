#!/bin/sh
#concatenates stop id file and creates a scv with stop_id and stop_name columns
outfile=$1
(head -1 stopgtfsbx.csv ; tail -n +2 -q stopgtfs*.csv) | awk -F, '{print $1","$2}' > "$outfile"

