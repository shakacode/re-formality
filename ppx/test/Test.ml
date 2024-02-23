let check Case.{ expected; actual } =
  let open Alcotest in
  check Case.testable "same string" expected actual
;;

let ok case = Alcotest.test_case case `Quick (fun () -> case |> Case.ok |> check)
let error case = Alcotest.test_case case `Quick (fun () -> case |> Case.error |> check)

let () =
  let open Alcotest in
  run
    "Ppx"
    [ ( "oks"
      , [ "Ok__FieldWithNoValidator"
        ; "Ok__FieldWithSyncValidator"
        ; "Ok__FieldWithAsyncValidatorInOnChangeMode"
        ; "Ok__FieldWithAsyncValidatorInOnBlurMode"
        ; "Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode"
        ; "Ok__FieldWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode"
        ; "Ok__FieldWithSyncValidatorAndDependentFieldAndFieldWithSyncValidator"
        ; "Ok__FieldWithSyncValidatorAndTwoDependentFieldsWithSyncValidators"
        ; "Ok__TwoFieldsWithNoValidators"
        ; "Ok__TwoFieldsWithSyncValidators"
        ; "Ok__TwoFieldsWithAsyncValidatorsInOnChangeMode"
        ; "Ok__TwoFieldsWithAsyncValidatorsInOnBlurMode"
        ; "Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnChangeMode"
        ; "Ok__TwoFieldsWithSyncValidatorAndFieldWithAsyncValidatorInOnBlurMode"
        ; "Ok__FieldWithSyncValidatorAndFieldWithNoValidator"
        ; "Ok__FieldWithSyncValidatorAndCollectionWithNoCollectionValidatorAndFieldWithSyncValidator"
        ; "Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithSyncValidator"
        ; "Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithSyncValidator"
        ; "Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode"
        ; "Ok__CollectionWithNoCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode"
        ; "Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnChangeMode"
        ; "Ok__CollectionWithCollectionValidatorAndFieldOfCollectionWithAsyncValidatorInOnBlurMode"
        ; "Ok__CollectionWithNoCollectionValidatorAndTwoFieldsOfCollectionWithSyncValidator"
        ; "Ok__Message"
        ; "Ok__SubmissionError"
        ; "Ok__Metadata"
        ; "Ok__Include"
        ; "Ok__LargeFormWithValidators"
        ]
        |> List.rev
        |> List.rev_map ok )
    ; ( "errors"
      , [ "Error__InputNotFound"; "Error__InputNotRecord" ]
        |> List.rev
        |> List.rev_map error )
    ]
;;
