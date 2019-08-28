#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# vim: ts=4 sw=4 expandtab:

import os, sys, subprocess, difflib
from threading import Timer

DECAF_JAR = os.path.join('..', 'target', 'decaf.jar')
TAC_JAR = os.path.join('S3', 'tac.jar')
TIMEOUT_SECONDS = 5
SHOW_DIFF = True
SHOW_COLOR = True
diff = difflib.Differ()

# check if two files are equal
def eq(expected: str, actual: str) -> bool:
    with open(expected, 'r') as f:
        expected_lines = [line.rstrip()
                          for line in f.readlines() if line.rstrip() != '']

    with open(actual, 'r') as f:
        actual_lines = [line.rstrip()
                        for line in f.readlines() if line.rstrip() != '']

    results = list(difflib.unified_diff(expected_lines, actual_lines, expected, actual))
    if len(results) == 0:
        print('\033[32mCORRECT\033[0m' if SHOW_COLOR else 'CORRECT')
        return True

    print('\033[31mWRONG\033[0m' if SHOW_COLOR else 'WRONG')
    if SHOW_DIFF:
        s = '\n'.join(results)
        print('\033[35m' + s + '\033[0m' if SHOW_COLOR else s)
    return False

# running a shell command
def run(cmd: [str], output_name: str = '', ignore_error: bool = False) -> bool:
    fw = subprocess.PIPE if output_name == '' else open(output_name, 'w')
    p = subprocess.Popen(cmd, shell=False, stdout=fw, stderr=fw)
    try:
        ret = p.wait(TIMEOUT_SECONDS)
    except subprocess.TimeoutExpired:
        print('\033[1;34mTIMEOUT\033[0m' if SHOW_COLOR else 'TIMEOUT')
        return False

    if not ignore_error and ret != 0:
        print('\033[1;37;41mERROR\033[0m' if SHOW_COLOR else 'ERROR')
        return False

    return True

# running decaf
def run_decaf(args: [str]) -> bool:
    return run(['java', '-jar', DECAF_JAR] + args)

# running tac virtual machine
def run_tac(tac: str, output: str) -> bool:
    return run(['java', '-jar', TAC_JAR, tac], output, True)

# running java virtual machine
def run_jvm(bytecode_dir: str, output: str) -> bool:
    return run(['java', '-cp', bytecode_dir, 'Main'], output, True)

# tester
class Tester:
    def __init__(self, test_set: str):
        self.test_set = test_set
        self.output_dir = os.path.join(test_set, 'output')
        if not os.path.exists(self.output_dir):
            os.mkdir(self.output_dir)
        self.result_dir = os.path.join(test_set, 'result')
        assert(os.path.isdir(self.result_dir))
        self.test_cases = [f for f in sorted(os.listdir(test_set)) if f.endswith('.decaf')]

    def get_test_case(self, test_case_name: str) -> str:
        return os.path.join(self.test_set, test_case_name)

    def get_output(self, test_case_name: str) -> str:
        return self.get_intermediate_output(test_case_name, '.output')

    def get_intermediate_output(self, test_case_name: str, ext: str) -> str:
        return os.path.join(self.output_dir, test_case_name.replace('.decaf', ext))

    def get_output_dir(self, test_case_name: str) -> str:
        d = os.path.join(self.output_dir, test_case_name.replace('.decaf', ''))
        if not os.path.exists(d):
            os.mkdir(d)
        return d

    def get_result(self, test_case_name: str) -> str:
        return os.path.join(self.result_dir, test_case_name.replace('.decaf', '.result'))

    def test_one(self, test_case_name: str) -> bool:
        raise NotImplementedError

    def test(self) -> [bool]:
        s = 'Running test set {}'.format(self.test_set)
        print('\033[4m' + s + '\033[0m' if SHOW_COLOR else s)
        return [self.test_one(t) for t in self.test_cases]

class PA1Or2Tester(Tester):
    def __init__(self, pa: str, test_set: str):
        self.pa = pa
        Tester.__init__(self, test_set)

    def test_one(self, test_case_name: str) -> bool:
        output = self.get_output(test_case_name)
        result = self.get_result(test_case_name)
        sys.stdout.write('{}  ...  '.format(test_case_name))
        # compile
        if run_decaf(['-t', self.pa, '-o', output, self.get_test_case(test_case_name)]):
            # compare output with result
            return eq(result, output)
        return False

class PA1Tester(PA1Or2Tester):
    def __init__(self, test_set: str):
        PA1Or2Tester.__init__(self, 'PA1', test_set)

class PA2Tester(PA1Or2Tester):
    def __init__(self, test_set: str):
        PA1Or2Tester.__init__(self, 'PA2', test_set)

class PA3Tester(Tester):
    def __init__(self, test_set: str):
        Tester.__init__(self, test_set)

    def test_one(self, test_case_name: str) -> bool:
        output = self.get_output(test_case_name)
        result = self.get_result(test_case_name)
        tac = self.get_intermediate_output(test_case_name, '.tac')
        sys.stdout.write('{}  ...  '.format(test_case_name))
        # compile
        if run_decaf(['-t', 'PA3', '-o', tac, self.get_test_case(test_case_name)]):
            # execute tac
            sys.stdout.write('TAC  ...  ')
            if run_tac(tac, output):
                # compare output with result
                return eq(result, output)
            return False
        return False

class JVMTester(Tester):
    def __init__(self, test_set: str):
        Tester.__init__(self, test_set)

    def test_one(self, test_case_name: str) -> bool:
        bytecode_dir = self.get_output_dir(test_case_name)
        output = self.get_output(test_case_name)
        result = self.get_result(test_case_name)
        sys.stdout.write('{}  ...  '.format(test_case_name))
        # compile
        if run_decaf(['-t', 'jvm', '-d', bytecode_dir, self.get_test_case(test_case_name)]):
            # execute bytecode
            sys.stdout.write('JVM  ...  ')
            if run_jvm(bytecode_dir, output):
                # compare output with result
                return eq(result, output)
            return False
        return False

TARGETS = {
    'PA1': (['S1'], PA1Tester),
    'PA2': (['S2'], PA2Tester),
    'PA3': (['S3'], PA3Tester),
    'jvm': (['S3-JVM'], JVMTester),
}

OPTIONS = ', '.join(TARGETS.keys())

def flatten(l):
    for el in l:
        if hasattr(el, '__iter__'):
            for sub in flatten(el):
                yield sub
        else:
            yield el

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Usage: ./testAll.py [{}]'.format(OPTIONS))
        sys.exit(1)

    if not os.path.exists(DECAF_JAR):
        raise RuntimeError('Cannot find decaf jar at path: ', DECAF_JAR)

    name = sys.argv[1]
    if name not in TARGETS:
        print('Invalid target: {}, options are: {}'.format(name, OPTIONS))

    (test_sets, tester) = TARGETS[name]
    results = list(flatten([tester(test_set).test() for test_set in test_sets]))
    succeeded = results.count(True)
    failed = results.count(False)
    s = 'Summary: {} succeeded, {} failed'.format(succeeded, failed)
    color = '\033[1;4;32m' if failed == 0 else '\033[1;4;31m'
    print(color + s + '\033[0m' if SHOW_COLOR else s)
