# Validation Strategies
The main purpose of this library is to provide great form validation UX. To achieve this, `Formality` follows the following principle:

<p align="center">
<strong>Validation feedback should be provided as soon as possible but not too soon.</strong>
</p>

The hardest part is to figure out the right moment when first validation results should be emitted in UI.

Let's break down a case with credit card field. A user opens a form and focuses on a field. When the first character is typed, the field is in an invalid state but it's not polite to show an error immediately: we should let user a chance to finish what he's doing. While the user is typing and field is in invalid state, we don't provide any feedback in UI. If after some character validator reported valid result, it's a proper moment to indicate a success (e.g. show credit card type). But if the user left the field in an invalid state (e.g. moved to another field) we have all rights to emit an error. After the first result is emitted, we update validation state in UI on every change.

Sadly, form fields are different and credit card scenario is not universal. This is where strategies kick in.

### Strategies
We can't have a single scenario for all the cases but we can spot the most common ones, describe a logic of each and apply proper scenarios to specific form fields. To understand a behavior of each strategy, add the following prefix to its name: _"Start providing feedback in UI on..."_

```reason
module Strategy = {
  type t =
    | OnFirstBlur
    | OnFirstChange
    | OnFirstSuccess
    | OnFirstSuccessOrFirstBlur
    | OnSubmit;
};
```

#### `OnFirstBlur`
Results are emitted on the first blur. After first results are emitted, a user receives feedback on every change in this field.

#### `OnFirstChange`
Results are emitted on the first change in a field (basically, as a user types).

#### `OnFirstSuccess`
Results are emitted on first successful validation. After first results are emitted, a user receives feedback on every change in this field.

#### `OnFirstSuccessOrFirstBlur` ✨
Results are emitted on first successful validation or on the first blur. After first results are emitted, a user receives feedback on every change in this field.

#### `OnSubmit`
Results are emitted only after the first submission attempt. After this, results for each field are emitted on every change until the form is reset.

---

Next: **[IO →](./03-IO.md)**
