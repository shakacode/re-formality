# PPX tests
These tests ensure that there are no errors/warnings during a compilation of valid forms and check proper error messages produced by PPX itself.

To run tests:

```shell
esy x test.exe
```

To inspect result produced by specific case:

```shell
test/script/output-read [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```

To write expected output for specific error case:

```shell
test/script/output-write [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```

To write un-ppx'ed source of a test case to sandbox for debugging:

```shell
test/script/sandbox [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```
