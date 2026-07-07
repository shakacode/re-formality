open Meta
open AstHelpers
open Ppxlib
open Ast_helper
module RestActions = Form_UseFormFn_RestActions
module CollectionsActions = Form_UseFormFn_CollectionsActions
module ApplyAsyncResultActions = Form_UseFormFn_ApplyAsyncResultActions
module BlurActions = Form_UseFormFn_BlurActions
module UpdateActions = Form_UseFormFn_UpdateActions

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
      (Uncurried.ty
         ~loc
         ~arity:2
         [%type: output -> (input, submissionError) submissionCallbacks -> unit])
  in
  let body =
    [%expr
      let memoizedInitialState =
        (React.useMemo1
           [%e
             Uncurried.fn
               ~loc
               ~arity:1
               [%expr fun () -> (initialState initialInput [@res.uapp])]]
           [| initialInput |] [@res.uapp])
      in
      let state, dispatch =
        let open ReactUpdate in
        (useReducer
           memoizedInitialState
           [%e
             Uncurried.fn
               ~loc
               ~arity:2
               [%expr
                 fun state action ->
                   [%e
                     Exp.match_
                       ~attrs:[ warning_4_disable ~loc ]
                       [%expr action]
                       (RestActions.ast ~loc ~async ~metadata
                        |> List.rev_append (CollectionsActions.ast ~loc ~metadata scheme)
                        |> List.rev_append (ApplyAsyncResultActions.ast ~loc scheme)
                        |> List.rev_append (BlurActions.ast ~loc ~metadata scheme)
                        |> List.rev_append (UpdateActions.ast ~loc ~metadata scheme))]]]
         [@res.uapp])
      in
      [%e Form_UseFormFn_Interface.ast ~scheme ~async ~metadata ~loc]]
  in
  [%stri
    let useForm =
      [%e
        Uncurried.fn
          ~loc
          ~arity:
            (match metadata with
             | Some _ -> 3
             | None -> 2)
          (Exp.fun_
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
              | None -> Exp.fun_ ~loc (Labelled "onSubmit") None on_submit_arg body))]
    ;;]
;;
