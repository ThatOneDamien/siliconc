#!/usr/bin/env python3

import argparse
import pathlib
import re
import shutil
import signal
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from enum import Enum


class StatusColor(Enum):
    PASS = '32'
    FAIL = '31'
    INFO = '34'


class DiagKind(Enum):
    FATAL = 'Fatal Error'
    ERROR = 'Error'
    WARNING = 'Warning'
    NOTE = 'Note'


def color_str(s: str, color: StatusColor):
    if sys.stdout.isatty():
        return f'\033[{color.value}m{s}\033[0m'
    return s


@dataclass(frozen=True)
class ExpectedDiag:
    kind: DiagKind
    message: str
    line_nr: int


@dataclass(frozen=True)
class CheckDirective:
    kind: str
    pattern: str
    line_nr: int


@dataclass
class TestOutcome:
    path: pathlib.PurePath
    passed: bool
    msg: str
    details: list[str]


DIAG_DIRECTIVE_RE = re.compile(r'// #([^:]+):\s+(\w.*)$')
DIAG_OUTPUT_RE = re.compile(r'[^:]+:(\d+):\d+: ([^:]+): (.*)')
CHECK_RE = re.compile(r'^\s*//\s*(CHECK(?:-NEXT|-NOT|-LABEL)?):\s*(.*?)\s*$')
TOKEN_RE = re.compile(r'\{\{(.*?)\}\}|\[\[([A-Za-z_]\w*)(?::(.*?))?\]\]')

DIRECTIVE_KINDS = {
    'fatal error': DiagKind.FATAL,
    'fatal': DiagKind.FATAL,
    'error': DiagKind.ERROR,
    'warning': DiagKind.WARNING,
    'note': DiagKind.NOTE,
}

OUTPUT_KINDS = {
    'fatal error': DiagKind.FATAL,
    'error': DiagKind.ERROR,
    'warning': DiagKind.WARNING,
    'note': DiagKind.NOTE,
}

SHORTHANDS = {
    'VAL': r'%[0-9A-Za-z_.]+',
    'GLOBAL': r'@[0-9A-Za-z_.]+',
    'INT': r'-?\d+',
}


def squash(s: str):
    return re.sub(r'\s+', ' ', s).strip()

def mangle(name: str):
    return f'@_SIF{len(name)}{name}'

def expand(expr: str):
    expr = re.sub(r'\$FN\((\w+)\)', lambda m: re.escape(mangle(m.group(1))), expr)
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
    lines = [squash(line) for line in ir_text.splitlines()]
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

    def report_nots(nots: list[CheckDirective], start: int, end: int):
        for n in nots:
            bad = find(n, start, end)
            if bad is not None:
                errors.append(f'{describe(n)} matched IR line: {lines[bad]}')

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
            report_nots(nots, pos, idx)
            nots = []
            pos = idx + 1
            has_prior = True
        report_nots(nots, pos, end)

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
    fail_msg: str
    details: list[str]

    def __init__(self, name: str):
        self.name = name
        self.expected = []
        self.checks = []
        self.ir_path = ''
        self.fail_msg = ''
        self.details = []

    def fail(self, msg: str):
        self.fail_msg = msg
        return False

    def validate(self):
        has_errors = any(d.kind in (DiagKind.ERROR, DiagKind.FATAL) for d in self.expected)
        if has_errors and self.checks:
            return ('test expects compile errors but also has CHECK directives; '
                    'split it into a diagnostics test and a codegen test')
        return None

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
            return self.fail('Compiler did not produce an IR file.')

        errors = run_checks(self.checks, ir_text)
        if not errors:
            return True

        self.fail('IR checks failed')
        self.details.extend(errors)
        return False

    def check_result(self, proc_res: subprocess.CompletedProcess[str]):
        unexpected: list[str] = []
        expected_count = len(self.expected)
        prev_success = True

        for line in proc_res.stderr.splitlines():
            m = DIAG_OUTPUT_RE.match(line)
            if m:
                line_nr = int(m.group(1))
                kind = OUTPUT_KINDS.get(m.group(2).lower())
                message = m.group(3)
                success = True
                if kind is not None:
                    found = self.find_diag(kind=kind, message=message, line_nr=line_nr)
                    if kind != DiagKind.NOTE:
                        success = found
                prev_success = success
                if not success:
                    unexpected.append(line)
            elif not prev_success:
                unexpected.append(line)

        has_more_expected = len(self.expected) > 0
        has_unexpected = len(unexpected) > 0
        if not has_more_expected and not has_unexpected:
            if expected_count > 0 and proc_res.returncode == 0:
                return self.fail('Compiler exited with 0 exit code despite errors.')
            if expected_count == 0 and proc_res.returncode != 0:
                return self.fail(f'Compiler exited with nonzero exit code ({proc_res.returncode}) despite no errors.')
            if self.checks and not self.check_ir():
                return False
            return True

        self.fail('Mismatched diagnostics')
        if has_more_expected:
            self.details.append(color_str('Diagnostics that were expected, but never occurred:', StatusColor.INFO))
            for diag in self.expected:
                self.details.append(f'    {diag.line_nr}: {diag.kind.value}: {diag.message}')
        if has_unexpected:
            self.details.append(color_str('Diagnostics that were unexpected:', StatusColor.INFO))
            for line in unexpected:
                self.details.append(f'    {line}')
        return False


def parse_test_file(test_path: str, test_name: str):
    test = TestMeta(test_name)
    with open(test_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    for i, raw in enumerate(lines):
        line = raw.strip()
        m = DIAG_DIRECTIVE_RE.search(line)
        if m:
            directive, message = m.groups()
            kind = DIRECTIVE_KINDS.get(directive)
            if kind is not None:
                line_nr = i + (2 if line.startswith('// #') else 1)
                test.expected.append(ExpectedDiag(kind=kind, message=message, line_nr=line_nr))

        cm = CHECK_RE.match(raw)
        if cm:
            test.checks.append(CheckDirective(kind=cm.group(1), pattern=cm.group(2), line_nr=i + 1))

    return test


class Node:
    def __init__(self, name: str):
        self.name = name
        self.children: dict[str, Node] = {}
        self.outcome: TestOutcome | None = None

    def counts(self):
        if self.outcome is not None:
            return (1 if self.outcome.passed else 0), 1
        passed = total = 0
        for child in self.children.values():
            p, t = child.counts()
            passed += p
            total += t
        return passed, total


def build_tree(outcomes: list[TestOutcome], root_name: str):
    root = Node(root_name)
    for o in outcomes:
        node = root
        for part in o.path.parts:
            node = node.children.setdefault(part, Node(part))
        node.outcome = o
    return root


def count_str(node: Node):
    p, t = node.counts()
    return color_str(f'{p}/{t}', StatusColor.PASS if p == t else StatusColor.FAIL)


def render_children(node: Node, prefix: str, verbose: bool, out: list[str]):
    kids = sorted(node.children.values(), key=lambda n: (n.outcome is not None, n.name))
    if not verbose:
        kids = [k for k in kids if k.outcome is None or not k.outcome.passed]

    for i, kid in enumerate(kids):
        last = i == len(kids) - 1
        branch = '└── ' if last else '├── '
        child_prefix = prefix + ('    ' if last else '│   ')
        o = kid.outcome
        if o is not None:
            if o.passed:
                out.append(f'{prefix}{branch}{color_str("✓", StatusColor.PASS)} {kid.name}')
            else:
                out.append(f'{prefix}{branch}{color_str("✗", StatusColor.FAIL)} {kid.name}: {o.msg}')
                for d in o.details:
                    out.append(f'{child_prefix}    {d}')
        else:
            out.append(f'{prefix}{branch}{kid.name}/ {count_str(kid)}')
            p, t = kid.counts()
            if verbose or p != t:
                render_children(kid, child_prefix, verbose, out)


def render_tree(root: Node, verbose: bool):
    out = [f'{root.name}/ {count_str(root)}']
    render_children(root, '', verbose, out)
    return '\n'.join(out)


class TestRunner:
    tests_passed: int
    test_count: int
    compiler_path: str
    test_root: str
    tmpdir: str
    outcomes: list[TestOutcome]

    def __init__(self, compiler_path: str, test_root: str, tmpdir: str):
        self.tests_passed = 0
        self.test_count = 0
        self.compiler_path = compiler_path
        self.test_root = test_root
        self.tmpdir = tmpdir
        self.outcomes = []

    def print_results(self):
        print('\n******** Result *********')
        tests_failed = self.test_count - self.tests_passed
        passed_str = color_str(f' {self.tests_passed} passed', StatusColor.PASS) if self.tests_passed != 0 else ''
        failed_str = color_str(f' {tests_failed} failed', StatusColor.FAIL) if tests_failed != 0 else ''
        separator = ',' if self.tests_passed != 0 and tests_failed != 0 else ''
        status = color_str('[PASS]', StatusColor.PASS) if tests_failed == 0 else color_str('[FAIL]', StatusColor.FAIL)
        print(f'{status} Completed {self.test_count} test(s):{passed_str}{separator}{failed_str}.')
        print('\n')

    def compile(self, test: TestMeta, test_path: str):
        ir_file = pathlib.Path(test.ir_path)
        if ir_file.exists():
            ir_file.unlink()

        cmd = [self.compiler_path, test_path, '-Wno-usage', '--out-name', 'test', '--emit', 'ir', '--out-dir', self.tmpdir]
        try:
            proc_res = subprocess.run(cmd, check=False, stdout=subprocess.PIPE,
                                      stderr=subprocess.PIPE, timeout=20, text=True)
        except subprocess.TimeoutExpired:
            return test.fail('Test timed out.')

        if proc_res.returncode < 0:
            sig = -proc_res.returncode
            try:
                sig_name = signal.Signals(sig).name
            except ValueError:
                sig_name = f'signal {sig}'
            test.fail(f'Compiler crashed with {sig_name}.')
            if proc_res.stderr:
                test.details.append(color_str('Extra stderr:', StatusColor.INFO))
                for line in proc_res.stderr.splitlines():
                    test.details.append(f'    {line}')
            return False

        return test.check_result(proc_res)

    def run_test_file(self, test_path: str):
        root = pathlib.Path(self.test_root)
        rel = pathlib.Path(test_path).relative_to(root) if root.is_dir() else pathlib.Path(root.name)
        test = parse_test_file(test_path, rel.as_posix())
        test.ir_path = str(pathlib.Path(self.tmpdir) / 'test.ll')
        self.test_count += 1

        problem = test.validate()
        if problem:
            passed = test.fail(problem)
        else:
            passed = self.compile(test, test_path)

        if passed:
            self.tests_passed += 1
        self.outcomes.append(TestOutcome(rel, passed, test.fail_msg, test.details))


def main():
    parser = argparse.ArgumentParser(description='Run tests for the silicon compiler.')
    parser.add_argument('compiler_path', metavar='<compiler path>', help='Path to the compiler binary.')
    parser.add_argument('test_root', metavar='<test file/dir>',
                        help='A test file, or a directory which is searched recursively for *.si tests.')
    parser.add_argument('-v', '--verbose', action='store_true', help='Show passing tests in the tree.')
    args = parser.parse_args()

    if not pathlib.Path(args.compiler_path).is_file():
        print(f'Failed to find compiler binary \'{args.compiler_path}\', try performing a make.', file=sys.stderr)
        sys.exit(1)

    print('******** Tests **********')
    tmpdir = tempfile.mkdtemp()
    tester = TestRunner(compiler_path=args.compiler_path, test_root=args.test_root, tmpdir=tmpdir)
    root = pathlib.Path(args.test_root)
    try:
        if root.is_file():
            tester.run_test_file(args.test_root)
        else:
            for test in sorted(root.rglob('*.si')):
                tester.run_test_file(str(test))
    finally:
        shutil.rmtree(tmpdir)

    if tester.test_count == 0:
        print('No tests found.')
        sys.exit(1)

    root_name = (root.parent.name if root.is_file() else root.name) or str(root)
    print(render_tree(build_tree(tester.outcomes, root_name), args.verbose))
    tester.print_results()
    sys.exit(0 if tester.tests_passed == tester.test_count else 1)


if __name__ == '__main__':
    main()
