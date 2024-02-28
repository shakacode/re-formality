# Contributing

## Requesting a feature
If you would like to request a feature, please, [create an issue](https://github.com/MinimaHQ/re-formality/issues/new).

## Reporting a bug
If you want to report a bug, there are few options:
1. You can contribute a minimal falling test with your use case. It would make things easier for us to proceed with the fix.
2. You can contribute a minimal falling test and a fix. Even better :)
3. If you don't feel like contributing a test, please, [create an issue](https://github.com/MinimaHQ/re-formality/issues/new) with as many details as possible and we will try to figure out something.

### Contributing a test
There might be 2 types of issues:
1. Compile-time error.
2. Runtime error (related to logic or just runtime crashes).

If you are facing the former, add PPX test in [`ppx/test`](./ppx/test).<br>
If you are facing the latter, add integration test in [`specs`](./specs).

See the corresponding README for details.

It would be great if you could reduce your test case to minimal size. I.e. instead of copy/pasting code from your app as is, try to remove unrelated parts and keep only what's related to the error.

## Technical details
### Repository structure
```shell
- docs/       # Documentation
- examples/   # Examples
- lib/        # ReScript library
  - src/      # ReScript library sources
- ppx/        # PPX
  - bin/      # PPX binary
  - lib/      # PPX implementation
  - test/     # PPX tests
- specs/      # Integration tests
```

### Setup
This repo uses `yarn` workspaces to manage frontend related dependencies and `opam` to manage PPX related dependencies (optionally, you can use `nix` shell instead of `opam` for development).

Install Yarn dependencies:

```shell
yarn install
```

Build ReScript library:

```shell
# In lib/ folder
yarn rescript build -with-deps
```

Build public interface of the ReScript lib:

```shell
# Apparently `rescript` doesn't have `bsb -install` counterpart
# So you need to build any app in this workspace that relies on `re-formality`

# E.g. in ./examples folder
yarn rescript build -with-deps
```

**Opam flow**
Install Esy dependencies:

```shell
opam init -a --disable-sandboxing --compiler=4.14.1
opam install . --deps-only --with-test
```

Build PPX:

```shell
opam exec -- dune build
```

**Nix/Devbox flow**
Considering you are already in Devbox shell, build PPX:

```shell
dune build
```
