# Metadata
Sometimes, to perform validation and return a proper type as an output you need something else besides a form state. For example, there is an array of categories `array(Category.t)` that comes from your server and you need to validate that input value is a valid category from this array. In such cases, you can add a `metadata` type to a form config and pass a value of this type to the `useForm` hook. Then all validators will receive an additional argument of this type on each invocation.

```reason
module Category = {
  type t = {
    id: CategoryId.t,
    name: string,
  }
};

module Form = [%form
  type input = {
    category: string,
  };

  type output = {
    category: Category.t,
  };

  type metadata = {
    categories: array(Category.t),
  };

  let validators = {
    category: {
      strategy: OnFirstChange,
      validate: (input, metadata) => {
        switch (input.category) {
        | "" => Error("Category is required")
        | _ =>
          let category =
            metadata.categories
            ->Belt.Array.getBy(category => category.name == input.category);
          switch category {
          | Some(category) => Ok(category)
          | None => Error("Invalid category")
          }
        };
      },
    },
  };
];

let initialInput = {category: ""};

[@react.component]
let make = (~categories: array(Category.t)) => {
  let form =
    Form.useForm(
      ~initialInput,
      ~metadata={categories: categories},
      ~onSubmit=(output, form) => { ... },
    );

  // ...
};
```

---

Next: **[Form Submission →](./09-FormSubmission.md)**
