open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) => {
  scheme
  |> T.record_of_fields(
       ~name="validators", ~loc, ~typ=(~validator, ~output_type) =>
       switch (validator) {
       | SyncValidator(Ok(Required))
       | SyncValidator(Ok(Optional(Some(_))))
       | SyncValidator(Error ()) => [%type:
           singleValueValidator(
             input,
             [%t output_type |> FieldType.unpack],
             message,
           )
         ]
       | SyncValidator(Ok(Optional(None))) => [%type: unit]
       | AsyncValidator(_) => [%type:
           Async.singleValueValidator(
             input,
             [%t output_type |> FieldType.unpack],
             message,
             action,
           )
         ]
       }
     );
};
