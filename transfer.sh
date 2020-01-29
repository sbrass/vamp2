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
function copy_files () {
    local src_dir="${1}"
    local dest_dir="${2}"
    shift 2
    for f in ${@}; do
        cp -a "${src_dir}/${f}" "${dest_dir}/${f}"
    done
}

for d in "src/utils" "src/rng" "src/vegas" "test"; do
    mkdir -p "${d}"
done

copy_files "${GIT}/basics" "src/utils" ${GIT_BASICS_FILES[@]}

copy_files "${SRC}/basics" "src/utils" ${BASICS_FILES[@]}
copy_files "${SRC}/utilities" "src/utils" ${UTILITIES_FILES[@]}
copy_files "${SRC}/system" "src/utils" ${SYSTEM_FILES[@]}
copy_files "${SRC}/rng"  "src/rng" ${RNG_FILES[@]}
copy_files "${SRC}/vegas" "src/vegas" ${VEGAS_FILES[@]}
copy_files "${SRC}/vegas" "test" ${VEGAS_UTI_FILES[@]}

