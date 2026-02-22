#!/usr/bin/env python3

import subprocess
import sys
import pathlib
import argparse
import tempfile
import shutil
import signal
import re
from enum import Enum
from dataclasses import dataclass

class StatusColor(Enum):
    PASS = "32"
    FAIL = "31"
    INFO = "34"

def color_str(s: str, color: StatusColor):
    if sys.stdout.isatty():
        return f'\033[{color.value}m{s}\033[0m'
    return s

@dataclass(frozen=True)
class ExpectedDiag:
    is_error: bool
    message: str
    line_nr: int

class TestResult:
    timeout: bool


class TestMeta:
    path: str
    expected: list[ExpectedDiag]

    def __init__(self, path: str):
        self.path = path
        self.expected = []

    def find_diag(self, is_error: bool, message: str, line_nr: int):
        for diag in self.expected:
            if diag.is_error == is_error and diag.line_nr == line_nr and diag.message in message:
                self.expected.remove(diag)
                return True

        return False

    def check_result(self, proc_res: subprocess.CompletedProcess[str]):
        lines = proc_res.stderr.splitlines()
        unexpected: list[str] = []
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            m = re.match(r'[^:]+:(\d+):\d+: (\w+): (.*)', line)
            if m:
                line_nr = int(m.group(1))
                kind = m.group(2).lower()
                message = m.group(3)
                success = True
                if kind == 'error':
                    success = self.find_diag(is_error=True, message=message, line_nr=line_nr)
                elif kind == 'warning':
                    success = self.find_diag(is_error=False, message=message, line_nr=line_nr)

                if not success:
                    unexpected.append(line)
            i += 1

        has_more_expected = len(self.expected) > 0
        has_unexpected = len(unexpected) > 0
        if not has_more_expected and not has_unexpected:
            print(color_str(f'[PASS] {self.path}', StatusColor.PASS))
            return True

        self.print_fail('Mismatched diagnostics')

        if has_more_expected:
            print(color_str('    Diagnostics that were expected, but never occurred:', StatusColor.INFO))
            for diag in self.expected:
                print(f'        {diag.line_nr}: {'Error' if diag.is_error else 'Warning'}: {diag.message}')
        if has_unexpected:
            print(color_str('    Diagnostics that were unexcpected:', StatusColor.INFO))
            for l in unexpected:
                print(f'        {l}')
        return False 

    def print_fail(self, msg: str):
        print(color_str(f'[FAIL] {self.path}: {msg}', StatusColor.FAIL))

class TestRunner:
    tests_passed: int
    test_count: int
    compiler_path: str
    tmpdir: str
    def __init__(self, compiler_path: str, tmpdir: str):
        self.tests_passed = 0
        self.test_count = 0
        self.compiler_path = compiler_path
        self.tmpdir = tmpdir


    def print_results(self):
        print('\n******** Result *********')
        passed_str = color_str(f'{self.tests_passed} passed', StatusColor.PASS)
        failed_str = color_str(f'{self.test_count - self.tests_passed} failed', StatusColor.FAIL)
        if self.test_count == self.tests_passed:
            print(f'{color_str('[PASS]', StatusColor.PASS)} Completed {self.test_count} test(s): {passed_str}, {failed_str}.')
        else:
            print(f'{color_str('[FAIL]', StatusColor.FAIL)} Completed {self.test_count} test(s): {passed_str}, {failed_str}.')
        print('\n')


    def run_test_file(self, test_path: str):
        test = parse_test_file(test_path)
        self.test_count += 1
        cmd = [self.compiler_path, test_path, '--emit', 'ir', '--out-dir', self.tmpdir]
        try:
            proc_res = subprocess.run(cmd, check=False, stdout=subprocess.PIPE,
                                      stderr=subprocess.PIPE, timeout=20, text=True)
            if proc_res.returncode < 0:
                sig = -proc_res.returncode
                try:
                    sig_name = signal.Signals(sig).name
                except ValueError:
                    sig_name = f'signal {sig}'
                test.print_fail(f'Compiler crashed with {sig_name}.')
            elif test.check_result(proc_res):
                self.tests_passed += 1
        except subprocess.TimeoutExpired:
            test.print_fail('Test timed out.')
            return



def parse_test_file(test_path: str):
    test = TestMeta(test_path)
    with open(test_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            m = re.search(r'// #(\w+):\s+(\w.*)$', line)
            if m:
                directive, message = m.groups()
                if directive == 'error':
                    test.expected.append(
                        ExpectedDiag(is_error=True, message=message, line_nr=i + (2 if line.startswith('// #') else 1))
                    )
                elif directive == 'warning':
                    test.expected.append(
                        ExpectedDiag(is_error=False, message=message, line_nr=i + (2 if line.startswith('// #') else 1))
                    )
            i += 1

    return test

def main():
    parser = argparse.ArgumentParser(description='Run tests for the silicon compiler.')
    parser.add_argument('compiler_path', metavar='<compiler path>', help='The test suite/file which will be run. All cases in the file will be run unless otherwise specified with -t.')
    parser.add_argument('test_root', metavar='<test file/dir>', help='The test suite/file which will be run. All cases in the file will be run unless otherwise specified with -t.')
    args = parser.parse_args()

    if not pathlib.Path(args.compiler_path).is_file():
        print(f'Failed to find compiler binary \'{args.compiler_path}\', try performing a make.', file=sys.stderr)
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
