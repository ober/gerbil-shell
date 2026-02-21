#!/usr/bin/env python3
"""Generate bash-compatibility.md from compat test results.

Runs all spec tests and produces a markdown report with per-test pass/fail
for both bash and gsh.

Usage: python3 test/gen_compat_report.py [--output FILE] OILS_DIR BASH GSH
"""

import json
import os
import sys
import datetime

# Import the test runner
sys.path.insert(0, os.path.dirname(__file__))
from run_spec import parse_test_file, run_test, check_result

# Test suites grouped by tier, matching the Makefile
TIERS = {
    'Tier 0 — Core': [
        'smoke', 'pipeline', 'redirect', 'redirect-multi',
        'builtin-eval-source', 'command-sub', 'comments', 'exit-status',
    ],
    'Tier 1 — Expansion & Variables': [
        'here-doc', 'quote', 'word-eval', 'word-split', 'var-sub',
        'var-sub-quote', 'var-num', 'var-op-test', 'var-op-strip',
        'var-op-len', 'assign', 'tilde',
    ],
    'Tier 2 — Builtins & Advanced': [
        'arith', 'glob', 'brace-expansion', 'case_', 'if_', 'loop',
        'for-expr', 'subshell', 'sh-func', 'builtin-echo', 'builtin-printf',
        'builtin-read', 'builtin-cd', 'builtin-set', 'builtin-type',
        'builtin-trap', 'builtin-bracket', 'builtin-misc', 'builtin-process',
        'background', 'command-parsing', 'var-op-bash', 'var-op-slice',
        'assign-extended',
    ],
}

# Friendly descriptions for each test suite
SUITE_DESCRIPTIONS = {
    'smoke': 'Basic shell operations',
    'pipeline': 'Pipe operator and pipelines',
    'redirect': 'I/O redirection (>, <, >>, etc.)',
    'redirect-multi': 'Multiple and complex redirections',
    'builtin-eval-source': 'eval and source/. builtins',
    'command-sub': 'Command substitution $() and ``',
    'comments': 'Shell comments',
    'exit-status': 'Exit status and $?',
    'here-doc': 'Here-documents (<<, <<-, <<< )',
    'quote': 'Quoting (single, double, $\'...\')',
    'word-eval': 'Word evaluation and expansion',
    'word-split': 'IFS word splitting',
    'var-sub': 'Variable substitution ($var, ${var})',
    'var-sub-quote': 'Variable substitution in quoting contexts',
    'var-num': 'Numeric/special variables ($#, $?, $$, etc.)',
    'var-op-test': 'Variable operators (${var:-default}, etc.)',
    'var-op-strip': 'Variable pattern stripping (${var#pat}, etc.)',
    'var-op-len': 'Variable length ${#var}',
    'assign': 'Variable assignment',
    'tilde': 'Tilde expansion (~, ~user)',
    'arith': 'Arithmetic expansion $(( )) and (( ))',
    'glob': 'Filename globbing (*, ?, [...])',
    'brace-expansion': 'Brace expansion ({a,b}, {1..5})',
    'case_': 'case statement',
    'if_': 'if/elif/else statement',
    'loop': 'while, until, for loops',
    'for-expr': 'C-style for ((i=0; ...))',
    'subshell': 'Subshell execution (...)',
    'sh-func': 'Shell functions',
    'builtin-echo': 'echo builtin',
    'builtin-printf': 'printf builtin',
    'builtin-read': 'read builtin',
    'builtin-cd': 'cd builtin',
    'builtin-set': 'set and shopt builtins',
    'builtin-type': 'type/command/which builtins',
    'builtin-trap': 'trap builtin',
    'builtin-bracket': '[[ ]] and [ ] test operators',
    'builtin-misc': 'Misc builtins (true, false, colon, etc.)',
    'builtin-process': 'Process builtins (kill, wait, ulimit, etc.)',
    'background': 'Background jobs (&, wait, jobs)',
    'command-parsing': 'Command parsing edge cases',
    'var-op-bash': 'Bash-specific variable operations',
    'var-op-slice': 'Variable slicing ${var:offset:length}',
    'assign-extended': 'declare/typeset/local/export',
}


def run_suite(spec_file, shells, spec_dir):
    """Run all tests in a spec file. Returns list of per-test results."""
    tests = parse_test_file(spec_file)
    shell_names = [os.path.basename(s) for s in shells]
    results = []

    for idx, test in enumerate(tests):
        test_num = idx + 1
        test_result = {
            'num': test_num,
            'name': test['name'],
            'shells': {},
        }
        bash_actual = None
        for si, (shell, sname) in enumerate(zip(shells, shell_names)):
            stdout, stderr, exit_code = run_test(test, shell, spec_dir)
            is_ref = (si == 0)
            if is_ref:
                bash_actual = (stdout, stderr, exit_code)
            passed, reason = check_result(test, stdout, stderr, exit_code,
                                          is_reference=is_ref,
                                          bash_actual=bash_actual)
            test_result['shells'][sname] = {
                'passed': passed,
                'reason': reason,
            }
        results.append(test_result)
    return results


def main():
    args = sys.argv[1:]
    output_file = None

    if '--output' in args:
        idx = args.index('--output')
        output_file = args[idx + 1]
        args = args[:idx] + args[idx + 2:]

    if len(args) < 3:
        print(f'Usage: {sys.argv[0]} [--output FILE] OILS_DIR BASH GSH',
              file=sys.stderr)
        sys.exit(1)

    oils_dir = args[0]
    shells = [os.path.abspath(s) for s in args[1:]]
    spec_dir = oils_dir

    # Collect all results
    all_results = {}  # suite_name -> list of test results
    grand_totals = {'bash': [0, 0], 'gsh': [0, 0]}  # [pass, total]

    for tier_name, suites in TIERS.items():
        for suite in suites:
            spec_file = os.path.join(oils_dir, 'spec', f'{suite}.test.sh')
            if not os.path.exists(spec_file):
                print(f'  SKIP {suite} (file not found)', file=sys.stderr)
                continue
            print(f'  Running {suite}...', file=sys.stderr)
            results = run_suite(spec_file, shells, spec_dir)
            all_results[suite] = results

            for r in results:
                for sname, info in r['shells'].items():
                    grand_totals[sname][1] += 1
                    if info['passed']:
                        grand_totals[sname][0] += 1

    # Generate markdown
    lines = []
    bp, bt = grand_totals['bash']
    gp, gt = grand_totals['gsh']
    bpct = (bp / bt * 100) if bt else 0
    gpct = (gp / gt * 100) if gt else 0

    lines.append('# Bash Compatibility Report')
    lines.append('')
    lines.append(f'Generated: {datetime.date.today().isoformat()}')
    lines.append('')
    lines.append('## Summary')
    lines.append('')
    lines.append(f'| Shell | Pass | Total | Rate |')
    lines.append(f'|-------|------|-------|------|')
    lines.append(f'| bash  | {bp} | {bt} | {bpct:.0f}% |')
    lines.append(f'| gsh   | {gp} | {gt} | {gpct:.0f}% |')
    lines.append('')

    # Per-tier summary
    lines.append('## Results by Tier')
    lines.append('')

    for tier_name, suites in TIERS.items():
        lines.append(f'### {tier_name}')
        lines.append('')
        lines.append(f'| Suite | Description | bash | gsh |')
        lines.append(f'|-------|-------------|------|-----|')

        for suite in suites:
            if suite not in all_results:
                continue
            results = all_results[suite]
            desc = SUITE_DESCRIPTIONS.get(suite, '')
            b_pass = sum(1 for r in results
                         if r['shells'].get('bash', {}).get('passed'))
            g_pass = sum(1 for r in results
                         if r['shells'].get('gsh', {}).get('passed'))
            total = len(results)
            b_str = f'{b_pass}/{total}' if b_pass < total else f'**{total}/{total}**'
            g_str = f'{g_pass}/{total}' if g_pass < total else f'**{total}/{total}**'
            lines.append(f'| {suite} | {desc} | {b_str} | {g_str} |')

        lines.append('')

    # Detailed failures section
    lines.append('## Failing Tests')
    lines.append('')
    lines.append('Tests where gsh fails but bash passes.')
    lines.append('')

    has_failures = False
    for tier_name, suites in TIERS.items():
        tier_failures = []
        for suite in suites:
            if suite not in all_results:
                continue
            for r in all_results[suite]:
                bash_ok = r['shells'].get('bash', {}).get('passed', False)
                gsh_ok = r['shells'].get('gsh', {}).get('passed', False)
                if bash_ok and not gsh_ok:
                    reason = r['shells']['gsh'].get('reason', '')
                    tier_failures.append((suite, r['num'], r['name'], reason))

        if tier_failures:
            has_failures = True
            lines.append(f'### {tier_name}')
            lines.append('')
            lines.append(f'| Suite | # | Test | Reason |')
            lines.append(f'|-------|---|------|--------|')
            for suite, num, name, reason in tier_failures:
                reason_short = reason[:80] + '...' if len(reason) > 80 else reason
                lines.append(f'| {suite} | {num} | {name} | {reason_short} |')
            lines.append('')

    if not has_failures:
        lines.append('*None — gsh passes all tests that bash passes.*')
        lines.append('')

    # Tests where gsh passes but bash fails
    lines.append('## Bonus: Tests where gsh passes but bash fails')
    lines.append('')
    bonus = []
    for suite, results in all_results.items():
        for r in results:
            bash_ok = r['shells'].get('bash', {}).get('passed', False)
            gsh_ok = r['shells'].get('gsh', {}).get('passed', False)
            if gsh_ok and not bash_ok:
                bonus.append((suite, r['num'], r['name']))
    if bonus:
        lines.append(f'| Suite | # | Test |')
        lines.append(f'|-------|---|------|')
        for suite, num, name in bonus:
            lines.append(f'| {suite} | {num} | {name} |')
    else:
        lines.append('*None.*')
    lines.append('')

    md = '\n'.join(lines)

    if output_file:
        with open(output_file, 'w') as f:
            f.write(md)
        print(f'Report written to {output_file}', file=sys.stderr)
    else:
        print(md)


if __name__ == '__main__':
    main()
