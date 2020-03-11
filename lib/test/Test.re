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

module Ok__FieldWithAsyncValidatorInOnChangeMode = {
  let case = "Ok__FieldWithAsyncValidatorInOnChangeMode";
  let run = () => case |> Case.ok |> check;
};

module Ok__FieldWithAsyncValidatorInOnBlurMode = {
  let case = "Ok__FieldWithAsyncValidatorInOnBlurMode";
  let run = () => case |> Case.ok |> check;
};

module Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode = {
  let case = "Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode";
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

module Ok__TwoFieldsWithAsyncValidatorsInOnChangeMode = {
  let case = "Ok__TwoFieldsWithAsyncValidatorsInOnChangeMode";
  let run = () => case |> Case.ok |> check;
};

module Ok__TwoFieldsWithAsyncValidatorsInOnBlurMode = {
  let case = "Ok__TwoFieldsWithAsyncValidatorsInOnBlurMode";
  let run = () => case |> Case.ok |> check;
};

module Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode = {
  let case = "Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode";
  let run = () => case |> Case.ok |> check;
};

module Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode = {
  let case = "Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode";
  let run = () => case |> Case.ok |> check;
};

module Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode = {
  let case = "Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode";
  let run = () => case |> Case.ok |> check;
};

module Ok__FieldWithSyncValidatorAndFieldWithNoValidator = {
  let case = "Ok__FieldWithSyncValidatorAndFieldWithNoValidator";
  let run = () => case |> Case.ok |> check;
};

module Ok__FieldWithSyncValidatorAndCollectionWithNoCollectionValidatorAndFieldWithSyncValidator = {
  let case = "Ok__FieldWithSyncValidatorAndCollectionWithNoCollectionValidatorAndFieldWithSyncValidator";
  let run = () => case |> Case.ok |> check;
};

module Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithSyncValidator = {
  let case = "Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithSyncValidator";
  let run = () => case |> Case.ok |> check;
};

module Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithSyncValidator = {
  let case = "Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithSyncValidator";
  let run = () => case |> Case.ok |> check;
};

module Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode = {
  let case = "Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode";
  let run = () => case |> Case.ok |> check;
};

module Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode = {
  let case = "Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode";
  let run = () => case |> Case.ok |> check;
};

module Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode = {
  let case = "Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode";
  let run = () => case |> Case.ok |> check;
};

module Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode = {
  let case = "Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode";
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
              Ok__FieldWithAsyncValidatorInOnChangeMode.case,
              `Quick,
              Ok__FieldWithAsyncValidatorInOnChangeMode.run,
            ),
            test_case(
              Ok__FieldWithAsyncValidatorInOnBlurMode.case,
              `Quick,
              Ok__FieldWithAsyncValidatorInOnBlurMode.run,
            ),
            test_case(
              Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode.case,
              `Quick,
              Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode.run,
            ),
            test_case(
              Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode.case,
              `Quick,
              Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode.run,
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
              Ok__TwoFieldsWithAsyncValidatorsInOnChangeMode.case,
              `Quick,
              Ok__TwoFieldsWithAsyncValidatorsInOnChangeMode.run,
            ),
            test_case(
              Ok__TwoFieldsWithAsyncValidatorsInOnBlurMode.case,
              `Quick,
              Ok__TwoFieldsWithAsyncValidatorsInOnBlurMode.run,
            ),
            test_case(
              Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode.case,
              `Quick,
              Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode.run,
            ),
            test_case(
              Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode.case,
              `Quick,
              Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode.run,
            ),
            test_case(
              Ok__FieldWithSyncValidatorAndFieldWithNoValidator.case,
              `Quick,
              Ok__FieldWithSyncValidatorAndFieldWithNoValidator.run,
            ),
            test_case(
              Ok__FieldWithSyncValidatorAndCollectionWithNoCollectionValidatorAndFieldWithSyncValidator.case,
              `Quick,
              Ok__FieldWithSyncValidatorAndCollectionWithNoCollectionValidatorAndFieldWithSyncValidator.run,
            ),
            test_case(
              Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithSyncValidator.case,
              `Quick,
              Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithSyncValidator.run,
            ),
            test_case(
              Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithSyncValidator.case,
              `Quick,
              Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithSyncValidator.run,
            ),
            test_case(
              Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode.case,
              `Quick,
              Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode.run,
            ),
            test_case(
              Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode.case,
              `Quick,
              Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode.run,
            ),
            test_case(
              Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode.case,
              `Quick,
              Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode.run,
            ),
            test_case(
              Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode.case,
              `Quick,
              Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode.run,
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
