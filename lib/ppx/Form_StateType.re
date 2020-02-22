open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc) => [%stri
  type state = {
    input,
    fieldsStatuses,
    collectionsStatuses,
    formStatus: formStatus(submissionError),
    submissionStatus,
  }
];
