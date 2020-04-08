#!/usr/bin/env bash

EXE_NAME=${1}
OUTPUT_NAME=${2}

shift 2

GPROF_TO_DOT_OPTIONS=(
    --edge-thres=0.05
    --node-thres=0.05
)

gprof ${EXE_NAME} ${OUTPUT_NAME} | gprof2dot ${GPROF_TO_DOT_OPTIONS[@]} | dot -Tpng -o ${OUTPUT_NAME}.png
