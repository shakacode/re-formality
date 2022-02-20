# PPX tests
These tests ensure that there are no errors/warnings during a compilation of valid forms and check proper error messages produced by PPX itself.

Before running tests:

```shell
# Install yarn deps
yarn install

# Install esy deps (unless you use nix)
esy install

# Build public interface of the lib
cd ppx/sandbox
yarn rescript build -with-deps
```

To run tests:

```shell
# with esy
esy x test.exe

#with nix
dune exec test.exe
```

To inspect result produced by specific case:

```shell
ppx/test/script/print-bsc-output [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```

To write expected output for specific error case:

```shell
ppx/test/script/write-error-snapshot [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```

To write un-ppx'ed source of a test case to sandbox for debugging:

```shell
ppx/test/script/sandbox [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```
