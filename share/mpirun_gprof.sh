#!/usr/bin/env bash

EXE_NAME=${1}

shift

# https://stackoverflow.com/a/464743
# GLIBC extends the prefix to {PREFIX}.{PID}
export GMON_OUT_PREFIX="${EXE_NAME}"
mpirun $@ ./${EXE_NAME} 2>&1 | tee ${EXE_NAME}.$(date -Iminutes).log
