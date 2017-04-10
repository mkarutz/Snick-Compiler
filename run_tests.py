import glob
import os
import subprocess

TEMP_TEST_FILE = "run_tests.out.tmp"
TEMP_DIFF_FILE = "run_tests.diff.tmp"

def get_tests():
    tests = []
    for file in glob.glob("tests/*.snick"):
        tests.append((file, file + ".out"))
    return tests

def run_test(test, expected):
    with open(TEMP_TEST_FILE, 'w') as out_file:
        subprocess.call(['./snick', '-p', test], stdout=out_file)
    with open(TEMP_DIFF_FILE, 'w') as diff_file:
        subprocess.call(['diff', expected, TEMP_TEST_FILE], stdout=diff_file)
    with open(TEMP_DIFF_FILE, 'r') as diff_file:
        diff = diff_file.read()
        return diff

def run_tests():
    print("Running tests")
    num_failures = 0
    num_success = 0
    for test, expected in get_tests():
        print("Running %s" % test)
        diff = run_test(test, expected)
        if (diff != ""):
            num_failures += 1
            print("FAILED: Output did not match expectation.")
            print(diff)
        else:
            print("PASSED")
            num_success += 1
    print("Tests finished: %d FAILED / %d PASSED"  % (num_failures, num_success))

def touch(file):
    fd = open(file, 'w')
    fd.close()

def setup():
    touch(TEMP_TEST_FILE)
    touch(TEMP_DIFF_FILE)

def teardown():
    os.remove(TEMP_TEST_FILE)
    os.remove(TEMP_DIFF_FILE)

setup()
run_tests()
teardown()