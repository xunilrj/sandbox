#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import os
from subprocess import Popen, PIPE


def solve_it(file_name):
    process = Popen(['cargo', "-q", 'run', "--release", '--', file_name],
                    stdout=PIPE, universal_newlines=True)
    (stdout, stderr) = process.communicate()
    return stdout.strip()


if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1:
        file_location = sys.argv[1].strip()
        print(solve_it(file_location))
    else:
        print('This test requires an input file.  Please select one from the data directory. (i.e. python solver.py ./data/gc_4_1)')
