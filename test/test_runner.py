#!/usr/bin/env python3

import subprocess
import sys
import pathlib
import argparse
import tempfile
import shutil
import re
from dataclasses import dataclass

@dataclass(frozen=True)
class Diagnostic:
    is_error: bool
    message: str
    line: int
    found: bool
    
@dataclass(frozen=True)
class TestResult:
    passed: bool
    message: str
    details: str

class TestMeta:
    diags: list[Diagnostic]

    def __init__(self):
        self.diags = []


class TestRunner:
    def __init__(self, compiler_path: str, tmpdir):
        self.tests_passed = 0
        self.test_count = 0
        self.compiler_path = compiler_path
        self.tmpdir = tmpdir


    def print_results(self):
        print('\n******** Result *********')
        completion_str = f'Completed {self.test_count} test(s): \033[32m{self.tests_passed}\033[0m passed, \033[31m{self.test_count - self.tests_passed}\033[0m failed.'
        if self.test_count == self.tests_passed:
            print_pass(completion_str)
        else:
            print_fail(completion_str)
        print('\n')


    def run_test_file(self, test_path: str):
        test = parse_test_file(test_path)
        self.test_count += 1
        result = self.run_test(test_path, test)
        if result.passed:
            self.tests_passed += 1
            print_pass(test_path)
        else:
            print_fail(test_path, result.details)


    def run_test(self, test_path: str, test: TestMeta):
        cmd = [self.compiler_path, test_path, '--emit', 'ir', '--out-dir', self.tmpdir]
        result = None
        result = subprocess.run(cmd, check=False, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, timeout=20, text=True)
        return check_test_result(test=test, proc_res=result)


def print_fail(test_name: str, details: str | None = None):
    print(f'\033[31m[FAIL]\033[0m {test_name}')
    if details:
        print('\n---- Details ----\n')
        print(details)
        print('-----------------\n')


def print_pass(test_name: str):
    print(f'\033[32m[PASS]\033[0m {test_name}')



def check_test_result(test: TestMeta, proc_res):
    result = TestResult(passed=True, message=None, details=None)
    rc = proc_res.returncode
    return result


def parse_test_file(test_path: str):
    test = TestMeta()
    with open(test_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            start = i
            m = re.search(r'// #(\w+):\s+(\w.*)$', line)
            if m:
                directive, message = m.groups()
                if directive == 'error':
                    test.diags.append(
                        Diagnostic(is_error=True, message=message, line=i + (2 if line.startswith('// #') else 1), found=False)
                    )
                elif directive == 'warning':
                    test.diags.append(
                        Diagnostic(is_error=False, message=message, line=i + (2 if line.startswith('// #') else 1), found=False)
                    )
                else:
                    print('Bad directive')
            i += 1

    return test

def main():
    parser = argparse.ArgumentParser(description='Run tests for the silicon compiler.')
    parser.add_argument('compiler_path', metavar='<compiler path>', help='The test suite/file which will be run. All cases in the file will be run unless otherwise specified with -t.')
    parser.add_argument('test_root', metavar='<test file/dir>', help='The test suite/file which will be run. All cases in the file will be run unless otherwise specified with -t.')
    args = parser.parse_args()

    if not pathlib.Path(args.compiler_path).is_file():
        print(f'Failed to find compiler binary \'{args.compiler_path}\', try performing a make.')
        sys.exit(1)

    print('******** Tests **********')
    tmpdir = tempfile.mkdtemp()
    tester = TestRunner(compiler_path=args.compiler_path, tmpdir=tmpdir)
    try:
        if pathlib.Path(args.test_root).is_file():
            tester.run_test_file(args.test_root)
        else:
            tests = sorted(
                pathlib.Path(args.test_root).rglob('*.si'),
                key=lambda p: p.name
            )
            for test in tests:
                tester.run_test_file(str(test))

    finally:
        shutil.rmtree(tmpdir)

    if tester.test_count == 0:
        print('No tests found.')
    else:
        tester.print_results()


if __name__ == '__main__':
    main()
