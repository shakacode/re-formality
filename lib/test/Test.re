let check = (Case.{expected, actual}) =>
  Alcotest.(check(Case.testable, "same string", expected, actual));

module InputNotFound = {
  let id = "input not found";
  let case = () => Case.run("Error__InputNotFound") |> check;
};

module InputNotRecord = {
  let id = "input not record";
  let case = () => Case.run("Error__InputNotRecord") |> check;
};

let () =
  Alcotest.(
    run(
      "Ppx",
      [
        (
          "errors",
          [
            test_case(InputNotFound.id, `Quick, InputNotFound.case),
            test_case(InputNotRecord.id, `Quick, InputNotRecord.case),
          ],
        ),
      ],
    )
  );
