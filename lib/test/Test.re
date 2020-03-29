let check = (Case.{expected, actual}) =>
  Alcotest.(check(Case.testable, "same string", expected, actual));

let ok = case => {
  Alcotest.test_case(case, `Quick, () => case |> Case.ok |> check);
};

let error = case => {
  Alcotest.test_case(case, `Quick, () => case |> Case.error |> check);
};

let () =
  Alcotest.(
    run(
      "Ppx",
      [
        (
          "oks",
          [
            "Ok__FieldWithNoValidator",
            "Ok__FieldWithSyncValidator",
            "Ok__FieldWithAsyncValidatorInOnChangeMode",
            "Ok__FieldWithAsyncValidatorInOnBlurMode",
            "Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode",
            "Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode",
            "Ok__FieldWithSyncValidatorAndDependentFieldAndFieldWithSyncValidator",
            "Ok__TwoFieldsWithNoValidators",
            "Ok__TwoFieldsWithSyncValidators",
            "Ok__TwoFieldsWithAsyncValidatorsInOnChangeMode",
            "Ok__TwoFieldsWithAsyncValidatorsInOnBlurMode",
            "Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode",
            "Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode",
            "Ok__FieldWithSyncValidatorAndFieldWithNoValidator",
            "Ok__FieldWithSyncValidatorAndCollectionWithNoCollectionValidatorAndFieldWithSyncValidator",
            "Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithSyncValidator",
            "Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithSyncValidator",
            "Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode",
            "Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode",
            "Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode",
            "Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode",
            "Ok__CollectionWithNoCollectionValidatorAndTwoFieldsOfCollectionWithSyncValidator",
            "Ok__Message",
            "Ok__SubmissionError",
            "Ok__Target",
            "Ok__Include",
          ]
          |> List.map(ok),
        ),
        (
          "errors",
          [
            "Error__InputNotFound",
            "Error__InputNotRecord",
            "Error__InvalidTarget",
          ]
          |> List.map(error),
        ),
      ],
    )
  );
