open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc) => [%stri
  let initialState = input => {
    input,
    fieldsStatuses: input->initialFieldsStatuses,
    collectionsStatuses: initialCollectionsStatuses,
    formStatus: Editing,
    submissionStatus: NeverSubmitted,
  }
];
