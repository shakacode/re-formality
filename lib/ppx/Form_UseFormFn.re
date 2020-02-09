open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, ~async: bool, scheme: Scheme.t) => [%stri
  let useForm =
      (
        ~initialInput: input,
        ~onSubmit:
           (output, submissionCallbacks(input, submissionError)) => unit,
      ) => {
    let memoizedInitialState =
      React.useMemo1(() => initialInput->initialState, [|initialInput|]);

    let (state, dispatch) =
      memoizedInitialState->ReactUpdate.useReducer((state, action) => {
        %e
        {
          Exp.match(
            [%expr action],
            Form_UseFormFn_RestActions.ast(~loc, ~async)
            |> List.append(
                 Form_UseFormFn_ApplyAsyncResultActions.ast(~loc, scheme),
               )
            |> List.append(Form_UseFormFn_BlurActions.ast(~loc, scheme))
            |> List.append(Form_UseFormFn_UpdateActions.ast(~loc, scheme)),
          );
        }
      });

    %e
    {
      Form_UseFormFn_Interface.ast(~loc, ~async, scheme);
    };
  }
];
