"""
Implements all CS 131-related test logic; is entry-point for testing framework.
"""

import asyncio
import importlib
from os import environ
import sys
import traceback
from operator import itemgetter

from harness import (
    AbstractTestScaffold,
    run_all_tests,
    get_score,
    write_gradescope_output,
)


class TestScaffold(AbstractTestScaffold):
    """Implement scaffold for Brewin' interpreter; load file, validate syntax, run testcase."""

    def __init__(self, interpreter_lib):
        self.interpreter_lib = interpreter_lib

    def setup(self, test_case):
        inputfile, expfile, srcfile = itemgetter("inputfile", "expfile", "srcfile")(
            test_case
        )

        with open(expfile, encoding="utf-8") as handle:
            expected = list(map(lambda x: x.rstrip("\n"), handle.readlines()))

        try:
            with open(inputfile, encoding="utf-8") as handle:
                stdin = list(map(lambda x: x.rstrip("\n"), handle.readlines()))
        except FileNotFoundError:
            stdin = None

        with open(srcfile, encoding="utf-8") as handle:
            program = handle.readlines()

        return {
            "expected": expected,
            "stdin": stdin,
            "program": program,
        }

    def run_test_case(self, test_case, environment):
        expect_failure = itemgetter("expect_failure")(test_case)
        stdin, expected, program = itemgetter("stdin", "expected", "program")(
            environment
        )
        interpreter = self.interpreter_lib.Interpreter(False, stdin, False)
        try:
            interpreter.validate_program(program)
            interpreter.run(program)
        except Exception as exception:  # pylint: disable=broad-except
            if expect_failure:
                error_type, line = interpreter.get_error_type_and_line()
                # received = [f"{error_type} {line}"]
                received = [f"{error_type}"]
                if received == expected:
                    return 1
                print("\nExpected error:")
                print(expected)
                print("\nReceived error:")
                print(received)

            print("\nException: ")
            print(exception)
            traceback.print_exc()
            return 0

        if expect_failure:
            print("\nExpected error:")
            print(expected)
            print("\nActual output:")
            print(interpreter.get_output())
            return 0

        passed = interpreter.get_output() == expected
        if not passed:
            print("\nExpected output:")
            print(expected)
            print("\nActual output:")
            print(interpreter.get_output())

        return int(passed)


def __generate_test_case_structure(
    cases, directory, category="", expect_failure=False, visible=lambda _: True
):
    return [
        {
            "name": f"{category} | {i}",
            "inputfile": f"{directory}{i}.in",
            "srcfile": f"{directory}{i}.brewin",
            "expfile": f"{directory}{i}.exp",
            "expect_failure": expect_failure,
            "visible": visible(f"test{i}"),
        }
        for i in cases
    ]


def __generate_test_suite(version, successes, failures):
    return __generate_test_case_structure(
        successes,
        f"v{version}/tests/",
        "Correctness",
        False,
    ) + __generate_test_case_structure(
        failures,
        f"v{version}/fails/",
        "Incorrectness",
        True,
    )


def generate_test_suite_v1():
    """wrapper for generate_test_suite for v1"""
    return __generate_test_suite(
        1,
        ["test_inputi", "test_recursion1", "test_set_field",  # passes
         "test_hello_world", "test_inputs", "test_addition", "test_while",  # added cases
         "test_comparisons1", "test_call_and_new", "test_equality", "test_return",
         "test_bool_operators", "test_factorial", "test_calls"],
        ["test_if", "test_incompat_operands1",  # fails
         "test_duplicate_classes", "test_duplicate_fields", "test_duplicate_methods",  # added cases
         "test_calling_undefined_method", "test_calling_undefined_method2",
         "test_set_unknown_field", "test_calling_with_nullptr", "test_new_unknown_class",
         "test_equality_incompat", "test_set_nothing"
         ],
    )


def generate_test_suite_v2():
    """wrapper for generate_test_suite for v2"""
    return __generate_test_suite(
        2,
        # passes
        ["test_compare_null", "test_return_default1",  "test_inher2", "test_inher1",
         "test_let", "test_simple", "test_scratch", "test_while3",
         "test_super", "test_return_default3", "test_inheri"],
        # fails
        ["test_incompat_return1", "test_let2", "test_inher1", "test_incompat_types2",
         "test_compare_unrelated_null", "test_scratch_fail", "test_inherits_unknown",
         "test_let_dupe", "test_bad_param", "test_bad_return"],
    )


def generate_test_suite_v3():
    """wrapper for generate_test_suite for v3"""
    return __generate_test_suite(
        3,
        # passes
        ["test_str_ops", "test_template1", "test_template8", "test_template7_check",
         "test_except1", "test_except13"],
        # fails
        ["test_except4", "test_incompat_template_types", "test_template5"])


async def main():
    """main entrypoint: argparses, delegates to test scaffold, suite generator, gradescope output"""
    if not sys.argv:
        raise ValueError("Error: Missing version number argument")
    version = sys.argv[1]
    module_name = f"interpreterv{version}"
    # module_name = f"interpreterv2"
    # module_name = f"interpreterv2 - original"
    interpreter = importlib.import_module(module_name)

    scaffold = TestScaffold(interpreter)

    match version:
        case "1":
            tests = generate_test_suite_v1()
        case "2":
            tests = generate_test_suite_v2()
        case "3":
            tests = generate_test_suite_v3()
        case _:
            raise ValueError("Unsupported version; expect one of 1,2,3")

    results = await run_all_tests(scaffold, tests)
    total_score = get_score(results) / len(results) * 100.0
    print(f"Total Score: {total_score:9.2f}%")

    # flag that toggles write path for results.json
    write_gradescope_output(results, environ.get("PROD", False))


if __name__ == "__main__":
    asyncio.run(main())
