open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, ~async, ~metadata) => [
  if (async) {
    Exp.case(
      [%pat? Submit],
      switch%expr (state.formStatus) {
      | Submitting(_) => NoUpdate
      | Editing
      | Submitted
      | SubmissionFailed(_) =>
        switch (
          switch%e (metadata) {
          | None =>
            %expr
            state.input
            ->validateForm(~validators, ~fieldsStatuses=state.fieldsStatuses)
          | Some () =>
            %expr
            state.input
            ->validateForm(
                ~validators,
                ~fieldsStatuses=state.fieldsStatuses,
                ~metadata,
              )
          }
        ) {
        | Validating({fieldsStatuses, collectionsStatuses}) =>
          Update({...state, fieldsStatuses, collectionsStatuses})
        | Valid({output, fieldsStatuses, collectionsStatuses}) =>
          UpdateWithSideEffects(
            {
              ...state,
              fieldsStatuses,
              collectionsStatuses,
              formStatus:
                Submitting(
                  switch (state.formStatus) {
                  | SubmissionFailed(error) => Some(error)
                  | Editing
                  | Submitted
                  | Submitting(_) => None
                  },
                ),
              submissionStatus: AttemptedToSubmit,
            },
            ({state: _, dispatch}) =>
              output->onSubmit({
                notifyOnSuccess: input => SetSubmittedStatus(input)->dispatch,
                notifyOnFailure: error =>
                  SetSubmissionFailedStatus(error)->dispatch,
                reset: () => Reset->dispatch,
                dismissSubmissionResult: () =>
                  DismissSubmissionResult->dispatch,
              }),
          )
        | Invalid({fieldsStatuses, collectionsStatuses}) =>
          Update({
            ...state,
            fieldsStatuses,
            collectionsStatuses,
            formStatus: Editing,
            submissionStatus: AttemptedToSubmit,
          })
        }
      },
    );
  } else {
    Exp.case(
      [%pat? Submit],
      switch%expr (state.formStatus) {
      | Submitting(_) => NoUpdate
      | Editing
      | Submitted
      | SubmissionFailed(_) =>
        switch (
          switch%e (metadata) {
          | None =>
            %expr
            state.input
            ->validateForm(~validators, ~fieldsStatuses=state.fieldsStatuses)
          | Some () =>
            %expr
            state.input
            ->validateForm(
                ~validators,
                ~fieldsStatuses=state.fieldsStatuses,
                ~metadata,
              )
          }
        ) {
        | Valid({output, fieldsStatuses, collectionsStatuses}) =>
          UpdateWithSideEffects(
            {
              ...state,
              fieldsStatuses,
              collectionsStatuses,
              formStatus:
                Submitting(
                  switch (state.formStatus) {
                  | SubmissionFailed(error) => Some(error)
                  | Editing
                  | Submitted
                  | Submitting(_) => None
                  },
                ),
              submissionStatus: AttemptedToSubmit,
            },
            ({state: _, dispatch}) =>
              output->onSubmit({
                notifyOnSuccess: input => SetSubmittedStatus(input)->dispatch,
                notifyOnFailure: error =>
                  SetSubmissionFailedStatus(error)->dispatch,
                reset: () => Reset->dispatch,
                dismissSubmissionResult: () =>
                  DismissSubmissionResult->dispatch,
              }),
          )
        | Invalid({fieldsStatuses, collectionsStatuses}) =>
          Update({
            ...state,
            fieldsStatuses,
            collectionsStatuses,
            formStatus: Editing,
            submissionStatus: AttemptedToSubmit,
          })
        }
      },
    );
  },
  Exp.case(
    [%pat? SetSubmittedStatus(input)],
    switch%expr (input) {
    | Some(input) =>
      Update({
        ...state,
        input,
        formStatus: Submitted,
        fieldsStatuses: input->initialFieldsStatuses,
      })
    | None =>
      Update({
        ...state,
        formStatus: Submitted,
        fieldsStatuses: state.input->initialFieldsStatuses,
      })
    },
  ),
  Exp.case(
    [%pat? SetSubmissionFailedStatus(error)],
    [%expr Update({...state, formStatus: SubmissionFailed(error)})],
  ),
  Exp.case(
    [%pat? MapSubmissionError(map)],
    switch%expr (state.formStatus) {
    | Submitting(Some(error)) =>
      Update({...state, formStatus: Submitting(Some(error->map))})
    | SubmissionFailed(error) =>
      Update({...state, formStatus: SubmissionFailed(error->map)})
    | Editing
    | Submitting(None)
    | Submitted => NoUpdate
    },
  ),
  Exp.case(
    [%pat? DismissSubmissionError],
    switch%expr (state.formStatus) {
    | Editing
    | Submitting(_)
    | Submitted => NoUpdate
    | SubmissionFailed(_) => Update({...state, formStatus: Editing})
    },
  ),
  Exp.case(
    [%pat? DismissSubmissionResult],
    switch%expr (state.formStatus) {
    | Editing
    | Submitting(_) => NoUpdate
    | Submitted
    | SubmissionFailed(_) => Update({...state, formStatus: Editing})
    },
  ),
  Exp.case([%pat? Reset], [%expr Update(initialInput->initialState)]),
];
