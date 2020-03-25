# Caveats

- [Warning 42 during compilation](#warning-42-during-compilation)

## Warning 42 during compilation
This warning is disabled by default but if you explicitly enabled it, you might see the following message during a compilation:

```
Warning 42: this use of [field] relies on type-directed disambiguation, it will not compile with OCaml 4.00 or earlier.
```

To get rid of it, consider to disable it either globally via `bsconfig.json` or locally in form modules:

```reason
[@ocaml.warning "-42"];
```

---

Next: **[API →](./11-API.md)**
