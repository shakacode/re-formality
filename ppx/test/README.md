# PPX tests
These tests ensure that there are no errors/warnings during a compilation of valid forms and check proper error messages produced by PPX itself.

Before running tests, setup the local env. See [CONTRIBUTING](/CONTRIBUTING.md) for more info.

To run tests:

```shell
## Opam
opam exec -- dune build
opam exec -- dune exec test.exe

## Nix
dune build
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
