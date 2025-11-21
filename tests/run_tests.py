#!/usr/bin/env python3

import subprocess
import sys
import pathlib
import argparse
import tempfile
import shutil
import configparser

TEST_ROOT = pathlib.Path(__file__).parent

def print_fail(test_name, message):
    print(f'\033[31m[FAIL]\033[0m {test_name}: {message}')

def print_pass(test_name, message):
    print(f'\033[32m[PASS]\033[0m {test_name}: {message}')

def check_test_result(test_name, should_pass, kind, proc_res):
    result = True
    rc = proc_res.returncode
    if rc != 0 and rc != 1: # Unknown exit 
        print_fail(test_name, f'{kind} encountered an unknown error/signal (Code: {rc})')
        result = False
    elif should_pass:
        if proc_res.returncode == 0:
            print_pass(test_name, f'{kind} finished successfully.')
        else:
            print_fail(test_name, f'{kind} unexpectedly failed.')
            result = False
    else:
        if proc_res.returncode == 1:
            print_pass(test_name, f'{kind} failed as expected.')
        else:
            print_fail(test_name, f'{kind} unexpectedly finished.')
            result = False
    return result


def run_test(test_path, compiler_path, tmpdir):
    test_name = test_path[test_path.rfind('/') + 1:]
    cfg_parser = configparser.ConfigParser()
    cfg_parser.read(test_path)
    action = cfg_parser['meta'].get('action', 'run')
    bhvr = cfg_parser['meta'].get('behavior', 'pass')
    should_pass = True
    if bhvr == 'pass':
        should_pass = True
    elif bhvr == 'fail':
        should_pass = False
    else:
        print(f'{test_name}: Test attribute \'behavior\' should only be either \'pass\' or \'fail\'')
        return

    timeout = cfg_parser['meta'].getint('timeout', 10)
    source = test_path + '.si'
    if action == 'compile':
        cmd = [compiler_path, '-s', source, '-o', tmpdir + 'a.o']
        result = subprocess.run(cmd, check=False, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, timeout=timeout, text=True)
        return check_test_result(test_name=test_name, should_pass=should_pass,
                                 kind='compiler', proc_res=result)
    elif action != 'run':
        print(f'{test_name}: Test attribute \'action\' should only be either \'run\' or \'compile\'')
        return

    cmd = [compiler_path, source, '-o', tmpdir + 'a.out']
    result = subprocess.run(cmd, check=False, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, timeout=timeout, text=True)
    if not check_test_result(test_name, True, 'compiler', result):
        return False
    cmd = [tmpdir + 'a.out']
    result = subprocess.run(cmd, check=False, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, timeout=timeout, text=True)
    return check_test_result(test_name=test_name, should_pass=should_pass,
                             kind='runtime', proc_res=result)


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

    print('******** Tests **********')
    tmpdir = tempfile.mkdtemp()
    test_count = 0
    tests_passed = 0
    try:
        if args.test:
            if not args.test.endswith('.test'):
                args.test = args.test + '.test'

            for test in TEST_ROOT.rglob(args.test):
                test_count += 1
                if run_test(str(test), str(compiler_path), tmpdir):
                    tests_passed += 1
        elif args.group:
            group_path = (TEST_ROOT/args.group).resolve()
            if not group_path.exists():
                print(f'Test group/directory \'{args.group}\' does not exist.')
                sys.exit(1)
            if not group_path.is_dir():
                print(f'Test group path \'{args.group}\' is not a directory.')
                sys.exit(1)
            for test in group_path.rglob('*.test'):
                test_count += 1
                if run_test(str(test), str(compiler_path), tmpdir):
                    tests_passed += 1
        else:
            for test in TEST_ROOT.rglob('*.test'):
                test_count += 1
                if run_test(str(test), str(compiler_path), tmpdir):
                    tests_passed += 1

    finally:
        shutil.rmtree(tmpdir)

    if test_count == 0:
        print('No tests found.')
        return
    print('\n******** Result *********')
    completion_str = f'Completed {test_count} test(s)'
    pass_fail_str = f'\033[31m{test_count - tests_passed}\033[0m failed, \033[32m{tests_passed}\033[0m passed.'
    if test_count == tests_passed:
        print_pass(completion_str, pass_fail_str)
    else:
        print_fail(completion_str, pass_fail_str)

if __name__ == '__main__':
    main()
