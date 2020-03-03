# PPX tests
These tests check only proper error messages produced by PPX itself.

To inspect error produced by specific case:

```shell
test/script/output-read [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```

To write expected output for specific case:

```shell
test/script/output-write [CASE_MODULE_NAME_WITHOUT_EXTENSION]
```

To run tests:

```shell
esy x test.exe
```
