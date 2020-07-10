open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~scheme: Scheme.t, ~async: bool, ~loc) => [%stri
  let useForm =
      (
        ~initialInput: input,
        ~onSubmit:
           (output, submissionCallbacks(input, submissionError)) => unit,
      ) => {
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
              Form_UseFormFn_RestActions.ast(~loc, ~async)
              |> List.rev_append(
                   Form_UseFormFn_CollectionsActions.ast(~loc, scheme),
                 )
              |> List.rev_append(
                   Form_UseFormFn_ApplyAsyncResultActions.ast(~loc, scheme),
                 )
              |> List.rev_append(
                   Form_UseFormFn_BlurActions.ast(~loc, scheme),
                 )
              |> List.rev_append(
                   Form_UseFormFn_UpdateActions.ast(~loc, scheme),
                 ),
            );
          }
        })
      );

    %e
    {
      Form_UseFormFn_Interface.ast(~scheme, ~async, ~loc);
    };
  }
];
