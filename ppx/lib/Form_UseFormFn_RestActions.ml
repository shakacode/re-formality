open Ppxlib
open Ast_helper

let ast ~loc ~async ~metadata =
  [ (if async
     then
       Exp.case
         [%pat? Submit]
         [%expr
           match state.formStatus with
           | Submitting _ -> NoUpdate
           | Editing | Submitted | SubmissionFailed _ ->
             (match
                [%e
                  match metadata with
                  | None ->
                    [%expr
                      (state.input |. validateForm)
                        ~validators
                        ~fieldsStatuses:state.fieldsStatuses]
                  | Some () ->
                    [%expr
                      (state.input |. validateForm)
                        ~validators
                        ~fieldsStatuses:state.fieldsStatuses
                        ~metadata]]
              with
              | Validating { fieldsStatuses; collectionsStatuses } ->
                Update { state with fieldsStatuses; collectionsStatuses }
              | Valid { output; fieldsStatuses; collectionsStatuses } ->
                UpdateWithSideEffects
                  ( { state with
                      fieldsStatuses
                    ; collectionsStatuses
                    ; formStatus =
                        Submitting
                          (match state.formStatus with
                           | SubmissionFailed error -> Some error
                           | Editing | Submitted | Submitting _ -> None)
                    ; submissionStatus = AttemptedToSubmit
                    }
                  , fun { state = _; dispatch } ->
                      (output |. onSubmit)
                        { notifyOnSuccess =
                            (fun input -> SetSubmittedStatus input |. dispatch)
                        ; notifyOnFailure =
                            (fun error -> SetSubmissionFailedStatus error |. dispatch)
                        ; reset = (fun () -> Reset |. dispatch)
                        ; dismissSubmissionResult =
                            (fun () -> DismissSubmissionResult |. dispatch)
                        } )
              | Invalid { fieldsStatuses; collectionsStatuses } ->
                Update
                  { state with
                    fieldsStatuses
                  ; collectionsStatuses
                  ; formStatus = Editing
                  ; submissionStatus = AttemptedToSubmit
                  })]
     else
       Exp.case
         [%pat? Submit]
         [%expr
           match state.formStatus with
           | Submitting _ -> NoUpdate
           | Editing | Submitted | SubmissionFailed _ ->
             (match
                [%e
                  match metadata with
                  | None ->
                    [%expr
                      (state.input |. validateForm)
                        ~validators
                        ~fieldsStatuses:state.fieldsStatuses]
                  | Some () ->
                    [%expr
                      (state.input |. validateForm)
                        ~validators
                        ~fieldsStatuses:state.fieldsStatuses
                        ~metadata]]
              with
              | Valid { output; fieldsStatuses; collectionsStatuses } ->
                UpdateWithSideEffects
                  ( { state with
                      fieldsStatuses
                    ; collectionsStatuses
                    ; formStatus =
                        Submitting
                          (match state.formStatus with
                           | SubmissionFailed error -> Some error
                           | Editing | Submitted | Submitting _ -> None)
                    ; submissionStatus = AttemptedToSubmit
                    }
                  , fun { state = _; dispatch } ->
                      (output |. onSubmit)
                        { notifyOnSuccess =
                            (fun input -> SetSubmittedStatus input |. dispatch)
                        ; notifyOnFailure =
                            (fun error -> SetSubmissionFailedStatus error |. dispatch)
                        ; reset = (fun () -> Reset |. dispatch)
                        ; dismissSubmissionResult =
                            (fun () -> DismissSubmissionResult |. dispatch)
                        } )
              | Invalid { fieldsStatuses; collectionsStatuses } ->
                Update
                  { state with
                    fieldsStatuses
                  ; collectionsStatuses
                  ; formStatus = Editing
                  ; submissionStatus = AttemptedToSubmit
                  })])
  ; Exp.case
      [%pat? SetSubmittedStatus input]
      [%expr
        match input with
        | Some input ->
          Update
            { state with
              input
            ; formStatus = Submitted
            ; fieldsStatuses = input |. initialFieldsStatuses
            }
        | None ->
          Update
            { state with
              formStatus = Submitted
            ; fieldsStatuses = state.input |. initialFieldsStatuses
            }]
  ; Exp.case
      [%pat? SetSubmissionFailedStatus error]
      [%expr Update { state with formStatus = SubmissionFailed error }]
  ; Exp.case
      [%pat? MapSubmissionError map]
      [%expr
        match state.formStatus with
        | Submitting (Some error) ->
          Update { state with formStatus = Submitting (Some (error |. map)) }
        | SubmissionFailed error ->
          Update { state with formStatus = SubmissionFailed (error |. map) }
        | Editing | Submitting None | Submitted -> NoUpdate]
  ; Exp.case
      [%pat? DismissSubmissionError]
      [%expr
        match state.formStatus with
        | Editing | Submitting _ | Submitted -> NoUpdate
        | SubmissionFailed _ -> Update { state with formStatus = Editing }]
  ; Exp.case
      [%pat? DismissSubmissionResult]
      [%expr
        match state.formStatus with
        | Editing | Submitting _ -> NoUpdate
        | Submitted | SubmissionFailed _ -> Update { state with formStatus = Editing }]
  ; Exp.case [%pat? Reset] [%expr Update (initialInput |. initialState)]
  ]
;;
