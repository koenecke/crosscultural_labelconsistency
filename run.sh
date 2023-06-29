#!/bin/bash

RAW_DATA="./rawData.csv"
LONG_DATA="./longData.csv"

Rscript requirements.R
Rscript readLong.R --path $RAW_DATA --output $LONG_DATA 