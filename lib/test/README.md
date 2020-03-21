# PPX tests
These tests ensure that there are no errors/warnings during a compilation of valid forms and check proper error messages produced by PPX itself.

Before running tests:

```shell
# Install yarn deps
yarn install

# Install esy deps
cd lib
esy install

# Build public interface of the lib
$(yarn bin)/bsb -install
```

To run tests:

```shell
esy x test.exe
```

To inspect result produced by specific case:

```shell
test/script/print-bsc-output [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```

To write expected output for specific error case:

```shell
test/script/write-error-snapshot [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```

To write un-ppx'ed source of a test case to sandbox for debugging:

```shell
test/script/sandbox [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```
