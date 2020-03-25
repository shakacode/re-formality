# Installation
To get started, add `re-formality` to your project either via `npm` or `yarn`:

```shell
# yarn
yarn add re-formality@next
# or npm
npm install --save re-formality@next
```

Under the hood, `re-formality` implemented as PPX. So you need to add it to both—`bs-dependencies` and `ppx-flags`—arrays in your `bsconfig.json`:

```json
"bs-dependencies": [
  "re-formality"
],
"ppx-flags": [
  "re-formality/ppx"
],
```

## Targets
Library supports 2 targets:
- `ReactDom`
- `ReactNative`

By default, it's set to `ReactDom`. But you can configure it in quite flexible ways:
1. If you want to apply specific target to all modules in the build, set environment variable `FORMALITY_TARGET` to chosen target:

```shell
FORMALITY_TARGET=ReactNative bsb -clean-world -make-world
```

2. If you want to set specific target on per module basis, do this in your form module:

```reason
module MyForm = [%form
  {target: ReactNative};

  type input = ...;
  type output = ...;
  ...
];
```

---

Before proceeding with actual code, we will elaborate on some core concepts that this library implements.

---

Next: **[Validation Strategies →](./02-ValidationStrategies.md)**
