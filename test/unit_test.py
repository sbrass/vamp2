#!/usr/bin/env python

from os import path
from os import sys

import subprocess
import difflib

assert(len(sys.argv) > 2), "Require two arguments: (i) path to executable, (ii) path to reference files (without .err/.out)."

ut_executable = sys.argv[1]
ut_reference_file = path.splitext(sys.argv[2])[0]
ut_reference_err = f"{ut_reference_file}.err"
ut_reference_out = f"{ut_reference_file}.out"

ut_log_err = f"{ut_executable}.err"
ut_log_out = f"{ut_executable}.out"

print(f"Unit test, execute {path.basename(ut_executable)} ...")
print(f"Logging to {ut_log_err} and {ut_log_out} ...")
with open(ut_log_err, "w") as log_err, open(ut_log_out, "w") as log_out:
    ut_run = subprocess.run(f"./{ut_executable}",
                            stderr=log_err,
                            stdout=log_out)

if ut_run.check_returncode () == None:
    print(f"... succeeded: {ut_run.returncode}")
else:
    print(f"... failed: {ut_run.returncode}")

def compare_log_and_ref (file_log, file_ref):
    with open(file_log, "r") as log, open(file_ref, "r") as ref:
        diff = difflib.context_diff(log.readlines(), ref.readlines())
        return len(list(diff)) == 0

def print_diff (file_log, file_ref):
    with open(file_log, "r") as log, open(file_ref, "r") as ref:
       diff = difflib.context_diff(log.readlines(), ref.readlines())
       sys.stderr.writelines(diff)

flag = ut_run.returncode == 0
diff_flag = compare_log_and_ref (ut_log_err, ut_reference_err)
if not diff_flag:
    print(f"Difference in {ut_log_err}...")
    print_diff(ut_log_err, ut_reference_err)
flag = flag and diff_flag

diff_flag = compare_log_and_ref (ut_log_out, ut_reference_out)
if not diff_flag:
    print(f"Difference in {ut_log_out}...")
    print_diff(ut_log_out, ut_reference_out)
flag = flag and diff_flag

# Forward exit-code.
if flag:
    sys.exit(0)
else:
    sys.exit(1)
