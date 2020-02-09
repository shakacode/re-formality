open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) =>
  scheme
  |> List.fold_left(
       (acc, entry: Scheme.entry) =>
         switch (entry) {
         | Field({validator: SyncValidator(_)}) => acc
         | Field({name, validator}) =>
           let field = Field.Field(name);
           [
             Exp.case(
               Pat.construct(
                 Lident(field |> Field.apply_async_result_action)
                 |> lid(~loc),
                 Some(
                   Pat.tuple([
                     Pat.var("value" |> str(~loc)),
                     Pat.var("result" |> str(~loc)),
                   ]),
                 ),
               ),
               {
                 %expr
                 {
                   let validator = [%e
                     field |> E.field(~of_="validators", ~loc)
                   ];
                   switch (
                     [%e
                       field
                       |> E.field2(~of_=("state", "fieldsStatuses"), ~loc)
                     ]
                   ) {
                   | Validating(x) when validator.eq(x, value) =>
                     Update({
                       ...state,
                       fieldsStatuses: [%e
                         field
                         |> E.update_field2(
                              ~of_=("state", "fieldsStatuses"),
                              ~with_=[%expr Dirty(result, Shown)],
                              ~loc,
                            )
                       ],
                     })
                   | Validating(_)
                   | Pristine
                   | Dirty(_, Shown | Hidden) => NoUpdate
                   };
                 };
               },
             ),
             ...acc,
           ];
         },
       [],
     );
