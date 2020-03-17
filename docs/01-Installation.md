# Installation
To get started, add `re-formality` to your project either via `npm` or `yarn`:

```shell
# yarn
yarn add re-formality
# or npm
npm install --save re-formality
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

Before proceeding with actual code, we will elaborate on some core concepts that this library implements.

---

Next: **[Validation Strategies →](./02-ValidationStrategies.md)**
