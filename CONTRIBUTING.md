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

If you are facing the former, add PPX test in [`lib/test`](./lib/test).<br>
If you are facing the latter, add integration test in [`specs`](./specs).

See the corresponding README for details.

It would be great if you could reduce your test case to minimal size. I.e. instead of copy/pasting code from your app as is, try to remove unrelated parts and keep only what's related to the error.

## Technical details
### Repository structure
```shell
- docs/       # Documentation
- examples/   # Examples
- lib/        # Library
  - bin/      # PPX binary
  - ppx/      # PPX sources
  - sandbox/  # Sandbox for PPX debugging
  - src/      # BuckleScript lib sources
  - test/     # PPX tests
- specs/      # Integration tests
```

### Setup
This repo uses `yarn` workspaces to manage frontend related dependencies and `esy` to manage PPX related dependencies.

Install dependencies:

```shell
# Install yarn deps
yarn install

# Install esy deps
cd lib
esy install
```

Build PPX:

```shell
# In lib folder
esy build
```

Build BuckleScript library:

```shell
# In lib folder
yarn bsb -clean-world -make-world
```

Build public interface of the BuckleScript lib:

```shell
# In lib folder
yarn bsb -install
```
