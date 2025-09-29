#!/usr/bin/env python3

import subprocess
import sys
import pathlib
import argparse
import tempfile
import shutil
import configparser

TEST_ROOT = pathlib.Path(__file__).parent
test_count = 0


def run(cmd, timeout=10, input_text=None):
    p = subprocess.run(cmd, input=input_text, check=False, stdout=subprocess.PIPE,
                       stderr=subprocess.PIPE, timeout=timeout, text=True)
    return p.returncode, p.stdout, p.stderr

def print_fail(test_name, message):
    print(f'\033[31m[FAIL]\033[0m {test_name}: {message}')

def print_pass(test_name, message):
    print(f'\033[32m[PASS]\033[0m {test_name}: {message}')

def check_test_result(test_name, should_pass, kind, exit_code, err):
    result = True
    if should_pass:
        if exit_code == 0:
            print_pass(test_name, f'{kind} finished successfully.')
        else:
            print_fail(test_name, f'{kind} unexpectedly fialed (Exit Code: {exit_code}).\n{err}')
            result = False
    else:
        if exit_code != 0:
            print_pass(test_name, f'{kind} failed as expected (Exit Code: {exit_code}).')
        else:
            print_fail(test_name, f'{kind} unexpectedly finished.')
            result = False
    return result


def run_test(test_path, compiler_path, tmpdir):
    global test_count
    test_count = test_count + 1
    test_name = test_path[test_path.rfind('/') + 1:]
    cfg_parser = configparser.ConfigParser()
    cfg_parser.read(test_path)
    action = cfg_parser['meta'].get('action', 'run')
    should_pass = cfg_parser['meta'].get('behavior', 'success') == 'success'
    timeout = cfg_parser['meta'].getint('timeout', 10)
    source = test_path + '.si'
    exit_code = None
    out = None
    err = None
    if action == 'compile':
        cmd = [compiler_path, '-s', source, '-o', tmpdir + 'a.o']
        exit_code, out, err = run(cmd, timeout)
        return check_test_result(test_name=test_name, should_pass=should_pass,
                                 kind='compiler', exit_code=exit_code, err=err)

    cmd = [compiler_path, source, '-o', tmpdir + 'a.out']
    exit_code, out, err = run(cmd, timeout)
    if exit_code != 0:
        print_fail(test_name, f'unable to run because of compiler failure (Exit Code {exit_code}).\n{err}')
        return False
    cmd = [tmpdir + 'a.out']
    exit_code, out, err = run(cmd, timeout)
    return check_test_result(test_name=test_name, should_pass=should_pass,
                             kind='runtime', exit_code=exit_code, err=err)

def main():
    compiler_path = ''
    parser = argparse.ArgumentParser()
    parser.add_argument('-t', '--test', help='Name of specific test to run.')
    parser.add_argument('-g', '--group', help='Name of test group (name of directory).')
    parser.add_argument('--release', action='count', help='Use release version of compiler executable.')
    args = parser.parse_args()

    if args.release:
        compiler_path = (TEST_ROOT/'../build/sic').resolve()
    else:
        compiler_path = (TEST_ROOT/'../build/sicdb').resolve()

    if not compiler_path.exists():
        print(f'Failed to find compiler binary \'{compiler_path}\', try performing a make.')
        sys.exit(1)

    tmpdir = tempfile.mkdtemp()
    try:
        if args.test:
            if not args.test.endswith('.test'):
                args.test = args.test + '.test'

            test_path = TEST_ROOT.rglob(args.test)
            has = False
            for test in test_path:
                has = True
                run_test(str(test), str(compiler_path), tmpdir)
            if not has:
                print(f'Test \'{args.test}\' does not exist')
        elif args.group:
            group_path = (TEST_ROOT/args.group).resolve()
            if not group_path.exists():
                print(f'Test group/directory \'{args.test}\' does not exist.')
                sys.exit(1)
            if not group_path.is_dir():
                print(f'Test group path \'{args.test}\' is not a directory.')
                sys.exit(1)

    finally:
        shutil.rmtree(tmpdir)
    print(f'Completed {test_count} tests.')

if __name__ == '__main__':
    main()
