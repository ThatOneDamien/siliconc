#!/usr/bin/env python3

import subprocess
import sys
import pathlib
import argparse
import tempfile
import shutil
import re

TEST_ROOT = pathlib.Path(__file__).parent.parent / 'tests'


class TestMeta:
    name: str
    source: str
    action: str
    should_pass: bool 
    output: str
    timeout: int

    def __init__(self):
        self.name = None
        self.source = None
        self.action = 'compile'
        self.should_pass = True
        self.output = None
        self.timeout = 10

class TestResult:
    passed: bool
    message: str
    details: str

class TestRunner:
    def __init__(self):
        self.tests_passed = 0
        self.test_count = 0


    def print_results(self):
        print('\n******** Result *********')
        completion_str = f'Completed {self.test_count} test(s)'
        pass_fail_str = f'\033[32m{self.tests_passed}\033[0m passed, \033[31m{self.test_count - self.tests_passed}\033[0m failed.'
        if self.test_count == self.tests_passed:
            print_pass(completion_str, pass_fail_str)
        else:
            print_fail(completion_str, pass_fail_str)
        print('\n')


    def run_test_file(self, test_path: str, compiler_path: str, tmpdir, verbose: bool, case: str | None = None):
        tests = parse_test_file(test_path)
        test_prefix = test_path[test_path.rfind('/') + 1:test_path.find('.test.si')] + ':'
        for test in tests:
            if not case or test.name.lower() == case.lower():
                self.test_count += 1
                result = self.run_test(test, compiler_path, tmpdir, verbose)
                if result.passed:
                    self.tests_passed += 1
                    print_pass(test_prefix + test.name, result.message)
                else:
                    print_fail(test_prefix + test.name, result.message, result.details)


    def run_test(self, test: TestMeta, compiler_path: str, tmpdir, verbose: bool):
        # Create tempfile with extracted source
        source_file = pathlib.Path(tmpdir) / (test.name + '.si')
        with open(source_file, 'w', encoding='utf-8') as f:
            f.write(test.source)

        # Check compile only test
        if test.action == 'compile':
            cmd = [compiler_path, source_file, '--emit', 'obj', '--out-dir', tmpdir]
            result = None
            if verbose:
                result = subprocess.run(cmd, check=False, timeout=test.timeout, text=True)
            else:
                result = subprocess.run(cmd, check=False, stdout=subprocess.PIPE,
                                        stderr=subprocess.PIPE, timeout=test.timeout, text=True)
            return check_test_result(should_pass=test.should_pass, kind='compiler', proc_res=result)

        # --- Compiling and Running ---

        # Compile first
        out_exe = pathlib.Path(tmpdir) / 'testout'
        cmd = [compiler_path, source_file, '--out-dir', tmpdir, '--out-name', 'testout']
        result = subprocess.run(cmd, check=False, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, timeout=test.timeout, text=True)
        if verbose:
            print(result.stdout)
        test_result = check_test_result(should_pass=True, kind='compiler', proc_res=result)
        if not test_result.passed:
            return test_result

        # Compilation succeeded, now run
        result = subprocess.run([out_exe], check=False, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, timeout=test.timeout, text=True)
        return check_test_result(should_pass=test.should_pass,
                                 kind='runtime', proc_res=result, output=test.output)


def print_fail(test_name: str, message: str, details: str | None = None):
    print(f'\033[31m[FAIL]\033[0m {test_name}: {message}')
    if details:
        print('\n---- Details ----\n')
        print(details)
        print('-----------------\n')


def print_pass(test_name: str, message: str):
    print(f'\033[32m[PASS]\033[0m {test_name}: {message}')



def check_test_result(should_pass: bool, kind: str, proc_res, output: str | None = None):
    result = TestResult()
    rc = proc_res.returncode
    if rc == 0:
        if not should_pass:
            result.passed  = False
            result.message = f'{kind} unexpectedly finished.'
            result.details = None
        elif output and output != proc_res.stdout.strip():
            result.passed  = False
            result.message = 'Failed to match expected output.'
            result.details = f'> {proc_res.stdout.strip()}\n< {output}\n'
        else:
            result.passed  = True
            result.message = f'{kind} finished successfully.'
            result.details = None
    elif rc == 1:
        if should_pass:
            result.passed  = False
            result.message = f'{kind} unexpectedly failed.'
            result.details = proc_res.stderr
        else:
            result.passed  = True
            result.message = f'{kind} failed as expected.'
            result.details = None
    elif rc == -11:
        result.passed  = False
        result.message = f'{kind} encountered a segfault.'
        result.details = proc_res.stderr

    else:
        result.passed  = False
        result.message = f'{kind} encountered an unknown error/signal (Code: {rc})'
        result.details = proc_res.stderr

    return result


def parse_test_file(test_path: str):
    tests = []
    test_set = set()
    with open(test_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    i = 0
    while i < len(lines):
        meta = TestMeta()
        start = i

        # Collect metadata lines starting with ///
        while i < len(lines) and lines[i].startswith('///'):
            m = re.match(r'^(\w+)\s*:\s*"?([^"]+)"?$', lines[i][3:].strip())
            if m:
                key, value = m.groups()
                key = key.lower()
                if key == 'test':
                    meta.name = value
                elif key == 'action':
                    value = value.lower()
                    if not value in ('compile', 'run'):
                        print(f'{test_path}:{i}: Test attribute \'action\' should only be either \'run\' or \'compile\'')
                        sys.exit(1)
                    meta.action = value
                elif key == 'result':
                    value = value.lower()
                    if not value in ('pass', 'fail'):
                        print(f'{test_path}:{i}: Test attribute \'result\' should only be either \'pass\' or \'fail\'')
                        sys.exit(1)
                    meta.should_pass = value == 'pass'
                elif key == 'output':
                    meta.output = value.strip()
                elif key == 'timeout':
                    meta.timeout = int(value)
                else:
                    print(f'{test_path}:{i}: Unrecognized test meta field \'{key}\', ignoring')
            i += 1

        # Skip empty lines between metadata and source
        while i < len(lines) and lines[i].strip() == '':
            i += 1

        # Collect source until next metadata block or EOF
        source_lines = []
        while i < len(lines) and not lines[i].startswith('///'):
            source_lines.append(lines[i].rstrip('\n'))
            i += 1

        meta.source = '\n'.join(source_lines).strip()

        if not meta.name:
            print(f'{test_path}:{start}: Test is missing a name')
            sys.exit(1)
        if not meta.source:
            print(f'{test_path}:{start}: Test \'{meta.name}\' has no source code')
            sys.exit(1)

        # Check for duplicate test names
        if meta.name in test_set:
            print(f'{test_path}:{start}: Duplicate test case name \'{meta.name}\'')
            sys.exit(1)

        test_set.add(meta.name)
        tests.append(meta)
    return tests

def main():
    compiler_path = ''
    parser = argparse.ArgumentParser(description='Run tests for the silicon compiler.')
    parser.add_argument('suite', nargs='?', help='The test suite/file which will be run. All cases in the file will be run unless otherwise specified with -t.')
    parser.add_argument('--release', action='store_true', help='Use release version of compiler executable.')
    parser.add_argument('-t', '--test', help='Specific test case within the specified test suite. Requires a suite to be specified.')
    parser.add_argument('-v', '--verbose', action='store_true', help='Show all compiler output')
    args = parser.parse_args()

    if args.release:
        compiler_path = (TEST_ROOT/'../build/sic').resolve()
    else:
        compiler_path = (TEST_ROOT/'../build/sicdb').resolve()

    if not compiler_path.exists():
        print(f'Failed to find compiler binary \'{str(compiler_path)}\', try performing a make.')
        sys.exit(1)

    compiler_path = str(compiler_path)

    print('******** Tests **********')
    tmpdir = tempfile.mkdtemp()
    tester = TestRunner()
    try:
        tests = None
        if args.suite:
            if not args.suite.endswith('.test.si'):
                args.suite += '.test.si'
            suite = TEST_ROOT / args.suite
            if not suite.exists():
                print(f'Test suite {args.suite} does not exist.')
                sys.exit(1)
            tester.run_test_file(str(suite), compiler_path, tmpdir, args.verbose, args.test)
        elif args.test:
            parser.error('Test argument should only be supplied if a suite is also supplied.')
        else:
            tests = sorted(
                TEST_ROOT.glob('*.test.si'),
                key=lambda p: p.name
            )
            for test in tests:
                tester.run_test_file(str(test), compiler_path, tmpdir, args.verbose)

    finally:
        shutil.rmtree(tmpdir)

    if tester.test_count == 0:
        print('No tests found.')
    else:
        tester.print_results()


if __name__ == '__main__':
    main()
