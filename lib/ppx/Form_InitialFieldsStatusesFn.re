open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast = (~loc, scheme: Scheme.t) => {
  [%stri
    let initialFieldsStatuses = (_input: input): fieldsStatuses => [%e
      Exp.record(
        scheme
        |> List.map((entry: Scheme.entry) =>
             (
               switch (entry) {
               | Field({name}) => Lident(name) |> lid(~loc)
               },
               [%expr Pristine],
             )
           ),
        None,
      )
    ]
  ];
};
