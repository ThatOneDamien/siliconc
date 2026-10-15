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

class DiagKind(Enum):
    FATAL = "Fatal Error"
    ERROR = "Error"
    WARNING = "Warning"
    NOTE = "Note"

def color_str(s: str, color: StatusColor):
    if sys.stdout.isatty():
        return f'\033[{color.value}m{s}\033[0m'
    return s

@dataclass(frozen=True)
class ExpectedDiag:
    kind: DiagKind 
    message: str
    line_nr: int

class TestResult:
    timeout: bool

@dataclass(frozen=True)
class CheckDirective:
    kind: str
    pattern: str
    line_nr: int

CHECK_RE = re.compile(r'^\s*//\s*(CHECK(?:-NEXT|-NOT|-LABEL)?):\s*(.*?)\s*$')
TOKEN_RE = re.compile(r'\{\{(.*?)\}\}|\[\[([A-Za-z_]\w*)(?::(.*?))?\]\]')

def squash(s: str):
    return re.sub(r'\s+', ' ', s).strip()

SHORTHANDS = {
    'VAL': r'%[0-9A-Za-z_.]+',     # local value / label name
    'GLOBAL': r'@[0-9A-Za-z_.]+',  # global / function name
    'INT': r'-?\d+',
}

def expand(expr: str):
    # Only known names are replaced, so a regex like `foo$` is untouched.
    return re.sub(r'\$([A-Z]+)', lambda m: SHORTHANDS.get(m.group(1), m.group(0)), expr)

def compile_pattern(pattern: str, variables: dict[str, str]):
    parts: list[str] = []
    defined: list[str] = []
    pos = 0

    def literal(text: str):
        if text:
            parts.append(re.escape(re.sub(r'\s+', ' ', text)))

    for m in TOKEN_RE.finditer(pattern):
        literal(pattern[pos:m.start()])
        pos = m.end()
        if m.group(1) is not None:
            parts.append(expand(m.group(1)))
        else:
            name, expr = m.group(2), m.group(3)
            if expr is not None:
                if expr == '':
                    expr = SHORTHANDS['VAL']
                parts.append(f'(?P<{name}>{expand(expr)})')
                defined.append(name)
            elif name in variables:
                parts.append(re.escape(variables[name]))
            else:
                raise ValueError(f'use of undefined variable [[{name}]]')
    literal(pattern[pos:])
    return re.compile(''.join(parts)), defined

def run_checks(directives: list[CheckDirective], ir_text: str):
    lines = [squash(l) for l in ir_text.splitlines()]
    variables: dict[str, str] = {}
    errors: list[str] = []

    def find(d: CheckDirective, start: int, end: int):
        regex, defined = compile_pattern(d.pattern, variables)
        for idx in range(start, end):
            m = regex.search(lines[idx])
            if m:
                for name in defined:
                    variables[name] = m.group(name)
                return idx
        return None

    def describe(d: CheckDirective):
        return f'line {d.line_nr}: {d.kind}: {d.pattern}'

    def run_block(checks: list[CheckDirective], pos: int, end: int, has_prior: bool):
        nots: list[CheckDirective] = []
        for d in checks:
            if d.kind == 'CHECK-NOT':
                nots.append(d)
                continue
            if d.kind == 'CHECK-NEXT':
                if not has_prior:
                    errors.append(f'{describe(d)} has no preceding match')
                    return
                idx = find(d, pos, min(pos + 1, end))
                if idx is None:
                    errors.append(f'{describe(d)} did not match the immediately following line')
                    return
            else:
                idx = find(d, pos, end)
                if idx is None:
                    errors.append(f'{describe(d)} found no match')
                    return
            for n in nots:
                bad = find(n, pos, idx)
                if bad is not None:
                    errors.append(f'{describe(n)} matched IR line: {lines[bad]}')
            nots = []
            pos = idx + 1
            has_prior = True
        for n in nots:
            bad = find(n, pos, end)
            if bad is not None:
                errors.append(f'{describe(n)} matched IR line: {lines[bad]}')

    groups: list[tuple[CheckDirective | None, list[CheckDirective]]] = [(None, [])]
    for d in directives:
        if d.kind == 'CHECK-LABEL':
            groups.append((d, []))
        else:
            groups[-1][1].append(d)

    try:
        starts = [0]
        cursor = 0
        for label, _ in groups[1:]:
            assert label is not None
            idx = find(label, cursor, len(lines))
            if idx is None:
                errors.append(f'{describe(label)} found no match')
                return errors
            starts.append(idx)
            cursor = idx + 1

        ends = starts[1:] + [len(lines)]
        for i, (label, checks) in enumerate(groups):
            pos = starts[i] + (1 if label is not None else 0)
            run_block(checks, pos, ends[i], label is not None)
    except (ValueError, re.error) as e:
        errors.append(f'invalid pattern: {e}')

    return errors

class TestMeta:
    name: str
    expected: list[ExpectedDiag]
    checks: list[CheckDirective]
    ir_path: str

    def __init__(self, name: str):
        self.name = name
        self.expected = []
        self.checks = []
        self.ir_path = ''

    def find_diag(self, kind: DiagKind, message: str, line_nr: int):
        for diag in self.expected:
            if diag.kind == kind and diag.line_nr == line_nr and diag.message in message:
                self.expected.remove(diag)
                return True

        return False

    def check_ir(self):
        try:
            with open(self.ir_path, 'r', encoding='utf-8') as f:
                ir_text = f.read()
        except OSError:
            self.print_fail('Compiler did not produce an IR file.')
            return False

        errors = run_checks(self.checks, ir_text)
        if not errors:
            return True

        self.print_fail('IR checks failed')
        for e in errors:
            print(f'        {e}')
        return False

    def check_result(self, proc_res: subprocess.CompletedProcess[str]):
        lines = proc_res.stderr.splitlines()
        unexpected: list[str] = []
        i = 0
        expected_count = len(self.expected)
        prev_success = True
        while i < len(lines):
            line = lines[i]
            m = re.match(r'[^:]+:(\d+):\d+: ([^:]+): (.*)', line)
            if m:
                line_nr = int(m.group(1))
                kind = m.group(2).lower()
                message = m.group(3)
                success = True
                if kind == 'fatal error':
                    success = self.find_diag(kind=DiagKind.FATAL, message=message, line_nr=line_nr)
                if kind == 'error':
                    success = self.find_diag(kind=DiagKind.ERROR, message=message, line_nr=line_nr)
                elif kind == 'warning':
                    success = self.find_diag(kind=DiagKind.WARNING, message=message, line_nr=line_nr)
                elif kind == 'note':
                    self.find_diag(kind=DiagKind.NOTE, message=message, line_nr=line_nr)

                prev_success = success
                if not success:
                    unexpected.append(line)
            elif not prev_success:
                unexpected.append(line)

            i += 1

        has_more_expected = len(self.expected) > 0
        has_unexpected = len(unexpected) > 0
        if not has_more_expected and not has_unexpected:
            if expected_count > 0 and proc_res.returncode == 0:
                self.print_fail('Compiler exited with 0 exit code despite errors.')
                return False
            elif expected_count == 0 and proc_res.returncode != 0:
                self.print_fail(f'Compiler exited with nonzero exit code({proc_res.returncode}) despite no errors.')
                return False
            if self.checks and not self.check_ir():
                return False
            print(color_str(f'[PASS] {self.name}', StatusColor.PASS))
            return True

        self.print_fail('Mismatched diagnostics')

        if has_more_expected:
            print(color_str('    Diagnostics that were expected, but never occurred:', StatusColor.INFO))
            for diag in self.expected:
                print(f'        {diag.line_nr}: {diag.kind.value}: {diag.message}')
        if has_unexpected:
            print(color_str('    Diagnostics that were unexcpected:', StatusColor.INFO))
            for l in unexpected:
                print(f'        {l}')
        return False 

    def print_fail(self, msg: str):
        print(color_str(f'[FAIL] {self.name}: {msg}', StatusColor.FAIL))

class TestRunner:
    tests_passed: int
    test_count: int
    compiler_path: str
    test_root: str
    tmpdir: str
    def __init__(self, compiler_path: str, test_root: str, tmpdir: str):
        self.tests_passed = 0
        self.test_count = 0
        self.compiler_path = compiler_path
        self.test_root = test_root
        self.tmpdir = tmpdir


    def print_results(self):
        print('\n******** Result *********')
        tests_failed = self.test_count - self.tests_passed
        passed_str = color_str(f' {self.tests_passed} passed', StatusColor.PASS) if self.tests_passed != 0 else ''
        failed_str = color_str(f' {tests_failed} failed', StatusColor.FAIL) if tests_failed != 0 else ''
        separator = ',' if self.tests_passed != 0 and tests_failed != 0 else ''

        if self.test_count == self.tests_passed:
            print(f'{color_str('[PASS]', StatusColor.PASS)} Completed {self.test_count} test(s):{passed_str}{separator}{failed_str}.')
        else:
            print(f'{color_str('[FAIL]', StatusColor.FAIL)} Completed {self.test_count} test(s):{passed_str}{separator}{failed_str}.')
        print('\n')


    def run_test_file(self, test_path: str):
        test = parse_test_file(test_path, self.test_root)
        test.ir_path = str(pathlib.Path(self.tmpdir) / 'test.ll')
        self.test_count += 1
        cmd = [self.compiler_path, test_path, '--out-name', 'test', '--emit', 'ir', '--out-dir', self.tmpdir]
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
                if proc_res.stderr:
                    print(color_str('    Extra stderr: ', StatusColor.INFO))
                    for line in proc_res.stderr.splitlines():
                        print(f'        {line}')
            elif test.check_result(proc_res):
                self.tests_passed += 1
        except subprocess.TimeoutExpired:
            test.print_fail('Test timed out.')
            return



def parse_test_file(test_path: str, test_root: str):
    test = TestMeta(test_path.removeprefix(test_root + '/'))
    with open(test_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        i = 0
        while i < len(lines):
            line = lines[i].strip()
            m = re.search(r'// #([^:]+):\s+(\w.*)$', line)
            if m:
                directive, message = m.groups()
                if directive == 'fatal error' or directive == 'fatal':
                    test.expected.append(
                        ExpectedDiag(kind=DiagKind.FATAL, message=message, line_nr=i + (2 if line.startswith('// #') else 1))
                    )
                elif directive == 'error':
                    test.expected.append(
                        ExpectedDiag(kind=DiagKind.ERROR, message=message, line_nr=i + (2 if line.startswith('// #') else 1))
                    )
                elif directive == 'warning':
                    test.expected.append(
                        ExpectedDiag(kind=DiagKind.WARNING, message=message, line_nr=i + (2 if line.startswith('// #') else 1))
                    )
                elif directive == 'note':
                    test.expected.append(
                        ExpectedDiag(kind=DiagKind.NOTE, message=message, line_nr=i + (2 if line.startswith('// #') else 1))
                    )

            cm = CHECK_RE.match(lines[i])
            if cm:
                test.checks.append(CheckDirective(kind=cm.group(1), pattern=cm.group(2), line_nr=i + 1))
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
    tester = TestRunner(compiler_path=args.compiler_path, test_root=args.test_root, tmpdir=tmpdir)
    try:
        if pathlib.Path(args.test_root).is_file():
            tester.run_test_file(args.test_root)
        else:
            tests = sorted(pathlib.Path(args.test_root).rglob('*.si'))
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
