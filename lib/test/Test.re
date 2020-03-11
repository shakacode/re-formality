let check = (Case.{expected, actual}) =>
  Alcotest.(check(Case.testable, "same string", expected, actual));

module Ok__FieldWithNoValidator = {
  let case = "Ok__FieldWithNoValidator";
  let run = () => case |> Case.ok |> check;
};

module Ok__FieldWithSyncValidator = {
  let case = "Ok__FieldWithSyncValidator";
  let run = () => case |> Case.ok |> check;
};

module Ok__FieldWithAsyncValidator = {
  let case = "Ok__FieldWithAsyncValidator";
  let run = () => case |> Case.ok |> check;
};

module Ok__FieldWithSyncValidatorAndFieldWithAsyncValidator = {
  let case = "Ok__FieldWithSyncValidatorAndFieldWithAsyncValidator";
  let run = () => case |> Case.ok |> check;
};

module Ok__TwoFieldsWithNoValidators = {
  let case = "Ok__TwoFieldsWithNoValidators";
  let run = () => case |> Case.ok |> check;
};

module Ok__TwoFieldsWithSyncValidators = {
  let case = "Ok__TwoFieldsWithSyncValidators";
  let run = () => case |> Case.ok |> check;
};

module Ok__TwoFieldsWithAsyncValidators = {
  let case = "Ok__TwoFieldsWithAsyncValidators";
  let run = () => case |> Case.ok |> check;
};

module Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidator = {
  let case = "Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidator";
  let run = () => case |> Case.ok |> check;
};

module Ok__FieldWithSyncValidatorAndFieldWithNoValidator = {
  let case = "Ok__FieldWithSyncValidatorAndFieldWithNoValidator";
  let run = () => case |> Case.ok |> check;
};

module Ok__FieldWithSyncValidatorsAndCollectionWithNoCollectionValidatorAndFieldWithSyncValidator = {
  let case = "Ok__FieldWithSyncValidatorsAndCollectionWithNoCollectionValidatorAndFieldWithSyncValidator";
  let run = () => case |> Case.ok |> check;
};

module Error__InputNotFound = {
  let case = "Error__InputNotFound";
  let run = () => case |> Case.error |> check;
};

module Error__InputNotRecord = {
  let case = "Error__InputNotRecord";
  let run = () => case |> Case.error |> check;
};

let () =
  Alcotest.(
    run(
      "Ppx",
      [
        (
          "oks",
          [
            test_case(
              Ok__FieldWithNoValidator.case,
              `Quick,
              Ok__FieldWithNoValidator.run,
            ),
            test_case(
              Ok__FieldWithSyncValidator.case,
              `Quick,
              Ok__FieldWithSyncValidator.run,
            ),
            test_case(
              Ok__FieldWithAsyncValidator.case,
              `Quick,
              Ok__FieldWithAsyncValidator.run,
            ),
            test_case(
              Ok__FieldWithSyncValidatorAndFieldWithAsyncValidator.case,
              `Quick,
              Ok__FieldWithSyncValidatorAndFieldWithAsyncValidator.run,
            ),
            test_case(
              Ok__TwoFieldsWithNoValidators.case,
              `Quick,
              Ok__TwoFieldsWithNoValidators.run,
            ),
            test_case(
              Ok__TwoFieldsWithSyncValidators.case,
              `Quick,
              Ok__TwoFieldsWithSyncValidators.run,
            ),
            test_case(
              Ok__TwoFieldsWithAsyncValidators.case,
              `Quick,
              Ok__TwoFieldsWithAsyncValidators.run,
            ),
            test_case(
              Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidator.case,
              `Quick,
              Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidator.run,
            ),
            test_case(
              Ok__FieldWithSyncValidatorAndFieldWithNoValidator.case,
              `Quick,
              Ok__FieldWithSyncValidatorAndFieldWithNoValidator.run,
            ),
            test_case(
              Ok__FieldWithSyncValidatorsAndCollectionWithNoCollectionValidatorAndFieldWithSyncValidator.case,
              `Quick,
              Ok__FieldWithSyncValidatorsAndCollectionWithNoCollectionValidatorAndFieldWithSyncValidator.run,
            ),
          ],
        ),
        (
          "errors",
          [
            test_case(
              Error__InputNotFound.case,
              `Quick,
              Error__InputNotFound.run,
            ),
            test_case(
              Error__InputNotRecord.case,
              `Quick,
              Error__InputNotRecord.run,
            ),
          ],
        ),
      ],
    )
  );
