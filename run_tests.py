import glob
import os
import subprocess

PPRINTER_TEST_DIR = "tests/pprinter/"
BRILL_TEST_DIR = "tests/brill/"

TEMP_COMPILER_OUT = "run_tests.brill.tmp"
TEMP_TEST_FILE = "run_tests.out.tmp"
TEMP_DIFF_FILE = "run_tests.diff.tmp"

def get_brill_tests():
    tests = []
    for fn in glob.glob(BRILL_TEST_DIR + "*.snick"):
        tests.append((fn, fn[:-6] + ".in", fn[:-6] + ".out"))
    return tests

def get_pprinter_tests():
    tests = []
    for fn in glob.glob(PPRINTER_TEST_DIR + "*.snick"):
        tests.append((fn, fn + ".out"))
    return tests
    
def run_brill_test(test):
    source_fn, input_fn, expected_fn = test
    
    # generate brill code
    with open(TEMP_COMPILER_OUT, 'w') as out_file:
        status = subprocess.call(['./snick', source_fn], stdout=out_file, stderr=out_file)
    
    if status == 1:
        # diff compiler output
        with open(TEMP_DIFF_FILE, 'w') as diff_file:
            subprocess.call([
                'diff', 
                '--ignore-trailing-space', 
                '--strip-trailing-cr', 
                expected_fn, 
                TEMP_COMPILER_OUT
            ], stdout=diff_file)
    if status == 0:
        # run brill code    
        with open(TEMP_TEST_FILE, 'w') as out_file:
            if not os.path.isfile(input_fn):
                subprocess.call([
                    './brill', 
                    TEMP_COMPILER_OUT
                ], stdout=out_file)
            else:
                with open(input_fn, 'r') as in_file:
                    subprocess.call([
                        './brill', 
                        TEMP_COMPILER_OUT
                    ], stdin=in_file, stdout=out_file)
    
        # diff program output
        with open(TEMP_DIFF_FILE, 'w') as diff_file:
            subprocess.call([
                'diff', 
                '--ignore-trailing-space', 
                '--strip-trailing-cr', 
                expected_fn, 
                TEMP_TEST_FILE
            ], stdout=diff_file)
            
    with open(TEMP_DIFF_FILE, 'r') as diff_file:
        diff = diff_file.read()
        return diff

def run_pprinter_test(test):
    source_fn, expected_fn = test
    
    with open(TEMP_TEST_FILE, 'w') as out_file:
        subprocess.call([
            './snick', 
            '-p', 
            source_fn
        ], stdout=out_file, stderr=out_file)
                        
    with open(TEMP_DIFF_FILE, 'w') as diff_file:
        subprocess.call([
            'diff', 
            '--ignore-trailing-space', 
            '--strip-trailing-cr', 
            expected_fn, 
            TEMP_TEST_FILE
        ], stdout=diff_file)
            
    with open(TEMP_DIFF_FILE, 'r') as diff_file:
        diff = diff_file.read()
        return diff

def run_tests():
    print("Running tests")
    print("===================================================================")
    num_failures = 0
    num_success = 0
    for test in get_pprinter_tests():
        print("Running test: %s" % test[0])
        diff = run_pprinter_test(test)
        if (diff != ""):
            num_failures += 1
            print("FAILED: Output did not match expectation.")
            print(diff)
        else:
            print("PASSED")
            num_success += 1
        print("")
        cleanup()
    
    for test in get_brill_tests():
        print("Running test: %s" % test[0])
        diff = run_brill_test(test)
        if (diff != ""):
            num_failures += 1
            print("FAILED: Output did not match expectation.")
            print(diff)
        else:
            print("PASSED")
            num_success += 1
        print("")
        cleanup()

    print("===================================================================")
    print("Tests finished: %d PASSED / %d FAILED"  
            % (num_success, num_failures))

def remove_ifexists(fn):
    if os.path.isfile(fn):
        os.remove(fn)

def cleanup():
    remove_ifexists(TEMP_TEST_FILE)
    remove_ifexists(TEMP_DIFF_FILE)
    remove_ifexists(TEMP_COMPILER_OUT)

if __name__ == "__main__":
    run_tests()
    cleanup()
