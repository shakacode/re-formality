open Meta
open AstHelpers
open Ppxlib
open Ast_helper

let ast ~(scheme : Scheme.t) ~(async : bool) ~(metadata : unit option) ~loc =
  let initial_input_arg = Pat.constraint_ ~loc [%pat? initialInput] [%type: input] in
  let metadata_arg =
    match metadata with
    | Some () -> Some (Pat.constraint_ ~loc [%pat? metadata] [%type: metadata])
    | None -> None
  in
  let on_submit_arg =
    Pat.constraint_
      ~loc
      [%pat? onSubmit]
      [%type: output -> (input, submissionError) submissionCallbacks -> unit]
  in
  let body =
    [%expr
      let memoizedInitialState =
        React.useMemo1 (fun () -> initialInput |. initialState) [| initialInput |]
      in
      let state, dispatch =
        let open ReactUpdate in
        (memoizedInitialState |. useReducer) (fun state action ->
          [%e
            Exp.match_
              ~attrs:[ warning_4_disable ~loc ]
              [%expr action]
              (Form_UseFormFn_RestActions.ast ~loc ~async ~metadata
               |> List.rev_append
                    (Form_UseFormFn_CollectionsActions.ast ~loc ~metadata scheme)
               |> List.rev_append (Form_UseFormFn_ApplyAsyncResultActions.ast ~loc scheme)
               |> List.rev_append (Form_UseFormFn_BlurActions.ast ~loc ~metadata scheme)
               |> List.rev_append (Form_UseFormFn_UpdateActions.ast ~loc ~metadata scheme)
              )])
      in
      [%e Form_UseFormFn_Interface.ast ~scheme ~async ~metadata ~loc]]
  in
  [%stri
    let useForm =
      [%e
        Exp.fun_
          ~loc
          (Labelled "initialInput")
          None
          initial_input_arg
          (match metadata_arg with
           | Some metadata_arg ->
             Exp.fun_
               ~loc
               (Labelled "metadata")
               None
               metadata_arg
               (Exp.fun_ ~loc (Labelled "onSubmit") None on_submit_arg body)
           | None -> Exp.fun_ ~loc (Labelled "onSubmit") None on_submit_arg body)]
    ;;]
;;
