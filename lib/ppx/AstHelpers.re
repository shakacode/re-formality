open Ast;
open Meta;

open Ppxlib;
open Ast_helper;

module T = {
  let constructor = (~loc, ~args: option(list(core_type))=?, x) =>
    Type.constructor(
      ~args=?
        switch (args) {
        | Some(args) => Some(Pcstr_tuple(args))
        | None => None
        },
      x |> str(~loc),
    );

  let record_of_fields =
      (
        ~name,
        ~loc,
        ~typ:
           (~validator: FieldValidator.t, ~output_type: FieldType.t) =>
           core_type,
        scheme: Scheme.t,
      ) =>
    name
    |> str(~loc)
    |> Type.mk(
         ~kind=
           Ptype_record(
             scheme
             |> List.map((entry: Scheme.entry) =>
                  switch (entry) {
                  | Field(field) =>
                    Type.field(
                      field.name |> str(~loc),
                      typ(
                        ~validator=field.validator,
                        ~output_type=field.output_type,
                      ),
                    )
                  }
                ),
           ),
       )
    |> StructureItem.from_type_declaration(~loc, ~rec_flag=Recursive);
};

module E = {
  let some = (~loc, x) =>
    Exp.construct(
      ~attrs=[explicit_arity(~loc)],
      Lident("Some") |> lid(~loc),
      Some(Exp.tuple([x])),
    );

  let ref_ = (~loc, x) =>
    Exp.apply(
      Exp.ident(Lident("!") |> lid(~loc)),
      [(Nolabel, Exp.ident(Lident(x) |> lid(~loc)))],
    );

  let rec seq = (~exp, ~make, list) =>
    switch (list) {
    | [] => exp
    | [x] => x |> make |> Exp.sequence(exp)
    | [x, ...rest] =>
      rest |> seq(~exp=x |> make |> Exp.sequence(exp), ~make)
    };

  let field = (~of_ as record, ~loc, field: Field.t) =>
    Exp.field(
      Exp.ident(Lident(record) |> lid(~loc)),
      switch (field) {
      | Field(field) => Lident(field) |> lid(~loc)
      },
    );

  let field2 = (~of_ as (record1, record2), ~loc, field: Field.t) =>
    Exp.field(
      Exp.field(
        Exp.ident(Lident(record1) |> lid(~loc)),
        Lident(record2) |> lid(~loc),
      ),
      switch (field) {
      | Field(field) => Lident(field) |> lid(~loc)
      },
    );

  let ref_field = (~of_ as record, ~loc, field: Field.t) =>
    Exp.field(
      record |> ref_(~loc),
      switch (field) {
      | Field(field) => Lident(field) |> lid(~loc)
      },
    );

  let update_field = (~of_ as record, ~with_ as value, ~loc, field: Field.t) =>
    Exp.record(
      [
        (
          switch (field) {
          | Field(field) => Lident(field) |> lid(~loc)
          },
          value,
        ),
      ],
      Some(Exp.ident(Lident(record) |> lid(~loc))),
    );

  let update_field2 =
      (~of_ as (record1, record2), ~with_ as value, ~loc, field: Field.t) =>
    Exp.record(
      [
        (
          switch (field) {
          | Field(field) => Lident(field) |> lid(~loc)
          },
          value,
        ),
      ],
      Some(
        Exp.field(
          Exp.ident(Lident(record1) |> lid(~loc)),
          Lident(record2) |> lid(~loc),
        ),
      ),
    );

  let update_ref_field =
      (~of_ as record, ~with_ as value, ~loc, field: Field.t) =>
    Exp.record(
      [
        (
          switch (field) {
          | Field(field) => Lident(field) |> lid(~loc)
          },
          value,
        ),
      ],
      Some(record |> ref_(~loc)),
    );

  let record = (~loc, xs: list((string, expression))) =>
    Exp.record(
      xs |> List.map(((name, expr)) => (Lident(name) |> lid(~loc), expr)),
      None,
    );
};
