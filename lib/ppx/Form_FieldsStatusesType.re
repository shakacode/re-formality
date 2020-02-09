open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) => {
  scheme
  |> T.record_of_fields(
       ~name="fieldsStatuses", ~loc, ~typ=(~validator, ~output_type) =>
       switch (validator) {
       | SyncValidator(_) => [%type:
           fieldStatus([%t output_type |> FieldType.unpack], message)
         ]
       | AsyncValidator(_) => [%type:
           Async.fieldStatus([%t output_type |> FieldType.unpack], message)
         ]
       }
     );
};
