open Meta;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~scheme: Scheme.t, ~async: bool, ~metadata: option(unit), ~loc) => {
  let initial_input_arg =
    Pat.constraint_(~loc, [%pat? initialInput], [%type: input]);

  let metadata_arg =
    switch (metadata) {
    | Some () =>
      Some(Pat.constraint_(~loc, [%pat? metadata], [%type: metadata]))
    | None => None
    };

  let on_submit_arg =
    Pat.constraint_(
      ~loc,
      [%pat? onSubmit],
      [%type: (output, submissionCallbacks(input, submissionError)) => unit],
    );

  let body = {
    %expr
    {
      let memoizedInitialState =
        React.useMemo1(() => initialInput->initialState, [|initialInput|]);
      let (state, dispatch) =
        ReactUpdate.(
          memoizedInitialState->useReducer((state, action) => {
            %e
            {
              Exp.match(
                ~attrs=[warning_4_disable(~loc)],
                [%expr action],
                Form_UseFormFn_RestActions.ast(~loc, ~async, ~metadata)
                |> List.rev_append(
                     Form_UseFormFn_CollectionsActions.ast(
                       ~loc,
                       ~metadata,
                       scheme,
                     ),
                   )
                |> List.rev_append(
                     Form_UseFormFn_ApplyAsyncResultActions.ast(~loc, scheme),
                   )
                |> List.rev_append(
                     Form_UseFormFn_BlurActions.ast(~loc, ~metadata, scheme),
                   )
                |> List.rev_append(
                     Form_UseFormFn_UpdateActions.ast(
                       ~loc,
                       ~metadata,
                       scheme,
                     ),
                   ),
              );
            }
          })
        );
      %e
      {
        Form_UseFormFn_Interface.ast(~scheme, ~async, ~metadata, ~loc);
      };
    };
  };

  [%stri
    let useForm = [%e
      Exp.fun_(
        ~loc,
        Labelled("initialInput"),
        None,
        initial_input_arg,
        switch (metadata_arg) {
        | Some(metadata_arg) =>
          Exp.fun_(
            ~loc,
            Labelled("metadata"),
            None,
            metadata_arg,
            Exp.fun_(~loc, Labelled("onSubmit"), None, on_submit_arg, body),
          )
        | None =>
          Exp.fun_(~loc, Labelled("onSubmit"), None, on_submit_arg, body)
        },
      )
    ]
  ];
};
