#!/usr/bin/env bash

set -x

SRC="${HOME}/whizard/mpi-request/src"

UTILITIES_FILES=(
    array_list.f90
    binary_tree.f90
    iterator.f90
    queue.f90
)

VEGAS_FILES=(
    partition.f90
    request_balancer.f90
    request_callback.f90
    request_caller.f90
    request_state.f90
    resources.f90
    channel_balancer.f90
)

function copy_files () {
    local src_dir="${1}"
    local dest_dir="${2}"
    shift 2
    for f in ${@}; do
        cp -a "${src_dir}/${f}" "${dest_dir}/${f}"
    done
}

for d in "src/vegas/mpi"; do
    mkdir -p "${d}"
done

copy_files "${SRC}/utilities" "src/utils" ${UTILITIES_FILES[@]}
copy_files "${SRC}/vegas" "src/vegas/mpi" ${VEGAS_FILES[@]}

