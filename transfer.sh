#!/usr/bin/env bash

set -x

GIT="${HOME}/whizard/development/src"
SRC="${HOME}/whizard/mpi-request/src"

GIT_BASICS_FILES=(
    constants.f90
    io_units.f90
)

BASICS_FILES=(
    kinds.f90
)

UTILITIES_FILES=(
    # format_utils.f90 # Replace with a lighter, minimal functional version.
    # numeric_utils.f90
    format_defs.f90 # check
)

SYSTEM_FILES=(
    # diagnostics.f90 # Replace with a lighter version.
)

RNG_FILES=(
    rng_base.f90   # check
    rng_stream.f90 # check
)

VEGAS_FILES=(
    # vegas.f90_serial
    # vegas.f90_mpi
    # vamp2.f90_serial # check
    # vamp2.f90_mpi    # check
)

VEGAS_UTI_FILES=(
    vegas_uti.f90
    vamp2_uti.f90
)

#
# Missing files: iso_varying_string
#
function copy_git_files () {
    local dir="${1}"
    shift
    for f in ${@}; do
        cp -a "${GIT}/${dir}/${f}" "src/${f}"
    done
}

function copy_src_files () {
    local dir="${1}"
    shift
    for f in ${@}; do
        cp -a "${SRC}/${dir}/${f}" "src/${f}"
    done
}

function copy_test_files () {
    local dir="${1}"
    shift
    for f in ${@}; do
        cp -a "${SRC}/${dir}/${f}" "test/${f}"
    done
}

copy_git_files "basics" ${GIT_BASICS_FILES[@]}

copy_src_files "basics" ${BASICS_FILES[@]}
copy_src_files "utilities" ${UTILITIES_FILES[@]}
copy_src_files "system" ${SYSTEM_FILES[@]}
copy_src_files "rng" ${RNG_FILES[@]}
copy_src_files "vegas" ${VEGAS_FILES[@]}
copy_test_files "vegas" ${VEGAS_UTI_FILES[@]}

