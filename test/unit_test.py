#!/usr/bin/env python

from os import path
from os import sys

import subprocess
import difflib

assert(len(sys.argv) > 2), "Require two arguments: (i) path to executable, (ii) path to reference files (without .err/.out)."

ut_executable = sys.argv[1]
ut_reference_file = sys.argv[2]
ut_reference_basename = path.splitext(path.basename(ut_reference_file))[0]
ut_log_file = f"{ut_reference_basename}.out"

print(f"Unit test, execute {path.basename(ut_executable)} ...")
print(f"Logging to {ut_log_file} ...")
with open(ut_log_file, "w") as log:
    ut_run = subprocess.run(f"./{ut_executable}",
                            stderr=log,
                            stdout=log)

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
diff_flag = compare_log_and_ref (ut_log_file, ut_reference_file)
if not diff_flag:
    print(f"Difference in {ut_log_file}...")
    print_diff(ut_log_file, ut_reference_file)
flag = flag and diff_flag

# Forward exit-code.
if flag:
    sys.exit(0)
else:
    sys.exit(1)
