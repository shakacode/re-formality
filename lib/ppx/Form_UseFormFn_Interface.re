open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, ~async: bool, scheme: Scheme.t) => {
  let base = [
    ("input", [%expr state.input]),
    ("status", [%expr state.formStatus]),
    (
      "dirty",
      [%expr
        () => [%e
          Exp.match(
            [%expr state.fieldsStatuses],
            [
              Exp.case(
                Pat.record(
                  scheme
                  |> List.map((entry: Scheme.entry) =>
                       (
                         switch (entry) {
                         | Field({name}) => Lident(name) |> lid(~loc)
                         },
                         [%pat? Pristine],
                       )
                     ),
                  Closed,
                ),
                [%expr false],
              ),
              Exp.case([%pat? _], [%expr true]),
            ],
          )
        ]
      ],
    ),
    (
      "valid",
      if (async) {
        %expr
        () =>
          switch (
            state.input
            ->validateForm(~validators, ~fieldsStatuses=state.fieldsStatuses)
          ) {
          | None => None
          | Some(Valid(_)) => Some(true)
          | Some(Invalid(_)) => Some(false)
          };
      } else {
        %expr
        () =>
          switch (
            state.input
            ->validateForm(~validators, ~fieldsStatuses=state.fieldsStatuses)
          ) {
          | Valid(_) => true
          | Invalid(_) => false
          };
      },
    ),
    (
      "submitting",
      switch%expr (state.formStatus) {
      | Submitting(_) => true
      | Editing
      | Submitted
      | SubmissionFailed(_) => false
      },
    ),
    ("submit", [%expr () => Submit->dispatch]),
    ("mapSubmissionError", [%expr map => MapSubmissionError(map)->dispatch]),
    (
      "dismissSubmissionError",
      [%expr () => DismissSubmissionError->dispatch],
    ),
    (
      "dismissSubmissionResult",
      [%expr () => DismissSubmissionResult->dispatch],
    ),
    ("reset", [%expr () => Reset->dispatch]),
  ];

  let update_fns =
    scheme
    |> List.map((entry: Scheme.entry) =>
         switch (entry) {
         | Field({name}) => (
             Field.(Field(name) |> update_fn),
             [%expr
               (
                 input =>
                   [%e
                     Exp.construct(
                       Lident(Field.(Field(name) |> update_action))
                       |> lid(~loc),
                       Some([%expr input]),
                     )
                   ]
                   ->dispatch
               )
             ],
           )
         }
       );

  let blur_fns =
    scheme
    |> List.map((entry: Scheme.entry) =>
         switch (entry) {
         | Field({name}) => (
             Field.(Field(name) |> blur_fn),
             [%expr
               (
                 () =>
                   [%e
                     Exp.construct(
                       Lident(Field.(Field(name) |> blur_action))
                       |> lid(~loc),
                       None,
                     )
                   ]
                   ->dispatch
               )
             ],
           )
         }
       );

  let result_fns =
    scheme
    |> List.map((entry: Scheme.entry) =>
         switch (entry) {
         | Field({name, validator}) => (
             Field.(Field(name) |> result_fn),
             switch (validator) {
             | SyncValidator(_) =>
               %expr
               (
                 () => {
                   exposeFieldResult(
                     [%e
                       Field.Field(name)
                       |> E.field2(~of_=("state", "fieldsStatuses"), ~loc)
                     ],
                   );
                 }
               )
             | AsyncValidator(_) =>
               %expr
               (
                 () => {
                   Async.exposeFieldResult(
                     [%e
                       Field.Field(name)
                       |> E.field2(~of_=("state", "fieldsStatuses"), ~loc)
                     ],
                   );
                 }
               )
             },
           )
         }
       );

  E.record(
    ~loc,
    result_fns
    |> List.append(blur_fns)
    |> List.append(update_fns)
    |> List.append(base),
  );
};
