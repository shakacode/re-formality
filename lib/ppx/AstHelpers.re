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
};

module P = {
  let rec or_ = (~pat, ~make, list) =>
    switch (list) {
    | [] => pat
    | [x] => x |> make |> Pat.or_(pat)
    | [x, ...rest] => rest |> or_(~pat=x |> make |> Pat.or_(pat), ~make)
    };
};

module E = {
  let some = (~loc, x) =>
    Exp.construct(
      ~attrs=[explicit_arity(~loc)],
      Lident("Some") |> lid(~loc),
      Some(Exp.tuple([x])),
    );

  let rec seq = (~exp, ~make, list) =>
    switch (list) {
    | [] => exp
    | [x] => x |> make |> Exp.sequence(exp)
    | [x, ...rest] =>
      rest |> seq(~exp=x |> make |> Exp.sequence(exp), ~make)
    };

  let rec conj = (~exp, ~make, ~loc, list) =>
    switch (list) {
    | [] => exp
    | [x] =>
      Exp.apply(
        Exp.ident(Lident("&&") |> lid(~loc)),
        [(Nolabel, exp), (Nolabel, x |> make(~loc))],
      )
    | [x, ...rest] =>
      rest
      |> conj(
           ~exp=
             Exp.apply(
               Exp.ident(Lident("&&") |> lid(~loc)),
               [(Nolabel, exp), (Nolabel, x |> make(~loc))],
             ),
           ~make,
           ~loc,
         )
    };

  let ref_ = (~loc, x) =>
    Exp.apply(
      Exp.ident(Lident("!") |> lid(~loc)),
      [(Nolabel, Exp.ident(Lident(x) |> lid(~loc)))],
    );

  let record = (~loc, xs: list((string, expression))) =>
    Exp.record(
      xs
      |> List.rev
      |> List.rev_map(((name, expr)) => (Lident(name) |> lid(~loc), expr)),
      None,
    );

  let field = (~in_ as record, ~loc, field) =>
    Exp.field(
      Exp.ident(Lident(record) |> lid(~loc)),
      Lident(field) |> lid(~loc),
    );

  let field2 = (~in_ as (record1, record2), ~loc, field) =>
    Exp.field(
      Exp.field(
        Exp.ident(Lident(record1) |> lid(~loc)),
        Lident(record2) |> lid(~loc),
      ),
      Lident(field) |> lid(~loc),
    );

  let field3 = (~in_ as (record1, record2, record3), ~loc, field) =>
    Exp.field(
      Exp.field(
        Exp.field(
          Exp.ident(Lident(record1) |> lid(~loc)),
          Lident(record2) |> lid(~loc),
        ),
        Lident(record3) |> lid(~loc),
      ),
      Lident(field) |> lid(~loc),
    );

  let field4 = (~in_ as (record1, record2, record3, record4), ~loc, field) =>
    Exp.field(
      Exp.field(
        Exp.field(
          Exp.field(
            Exp.ident(Lident(record1) |> lid(~loc)),
            Lident(record2) |> lid(~loc),
          ),
          Lident(record3) |> lid(~loc),
        ),
        Lident(record4) |> lid(~loc),
      ),
      Lident(field) |> lid(~loc),
    );

  let field_of_collection =
      (~in_, ~collection: Collection.t, ~loc, field_name) =>
    Exp.field(
      Exp.apply(
        [%expr Belt.Array.getUnsafe],
        [
          (Nolabel, collection.plural |> field(~in_, ~loc)),
          (Nolabel, [%expr index]),
        ],
      ),
      Lident(field_name) |> lid(~loc),
    );

  let field_of_collection2 =
      (~in_, ~collection: Collection.t, ~loc, field_name) =>
    Exp.field(
      Exp.apply(
        [%expr Belt.Array.getUnsafe],
        [
          (Nolabel, collection.plural |> field2(~in_, ~loc)),
          (Nolabel, [%expr index]),
        ],
      ),
      Lident(field_name) |> lid(~loc),
    );

  let ref_field = (~in_ as record, ~loc, field) =>
    Exp.field(record |> ref_(~loc), Lident(field) |> lid(~loc));

  let ref_field2 = (~in_ as (record1, record2), ~loc, field) =>
    Exp.field(
      Exp.field(record1 |> ref_(~loc), Lident(record2) |> lid(~loc)),
      Lident(field) |> lid(~loc),
    );

  let ref_field_of_collection =
      (~in_ as record, ~collection: Collection.t, ~loc, field_name) =>
    Exp.field(
      Exp.apply(
        [%expr Belt.Array.getUnsafe],
        [
          (
            Nolabel,
            Exp.field(
              record |> ref_(~loc),
              Lident(collection.plural) |> lid(~loc),
            ),
          ),
          (Nolabel, [%expr index]),
        ],
      ),
      Lident(field_name) |> lid(~loc),
    );

  let apply_field = (~in_, ~fn, ~args, ~loc) =>
    Exp.apply(field(~in_, ~loc, fn), args);

  let apply_field2 = (~in_, ~fn, ~args, ~loc) =>
    Exp.apply(field2(~in_, ~loc, fn), args);

  let apply_field3 = (~in_, ~fn, ~args, ~loc) =>
    Exp.apply(field3(~in_, ~loc, fn), args);

  let apply_field4 = (~in_, ~fn, ~args, ~loc) =>
    Exp.apply(field4(~in_, ~loc, fn), args);

  let update_field = (~in_ as record, ~with_ as value, ~loc, field) =>
    Exp.record(
      [(Lident(field) |> lid(~loc), value)],
      Some(Exp.ident(Lident(record) |> lid(~loc))),
    );

  let update_field2 =
      (~in_ as (record1, record2), ~with_ as value, ~loc, field) =>
    Exp.record(
      [(Lident(field) |> lid(~loc), value)],
      Some(
        Exp.field(
          Exp.ident(Lident(record1) |> lid(~loc)),
          Lident(record2) |> lid(~loc),
        ),
      ),
    );

  let update_field3 =
      (~in_ as (record1, record2, record3), ~with_ as value, ~loc, field) =>
    Exp.record(
      [(Lident(field) |> lid(~loc), value)],
      Some(
        Exp.field(
          Exp.field(
            Exp.ident(Lident(record1) |> lid(~loc)),
            Lident(record2) |> lid(~loc),
          ),
          Lident(record3) |> lid(~loc),
        ),
      ),
    );

  let update_ref_field = (~in_ as record, ~with_ as value, ~loc, field) =>
    Exp.record(
      [(Lident(field) |> lid(~loc), value)],
      Some(record |> ref_(~loc)),
    );

  let update_ref_field2 =
      (~in_ as (record1, record2), ~with_ as value, ~loc, field) =>
    Exp.record(
      [(Lident(field) |> lid(~loc), value)],
      Some(
        Exp.field(record1 |> ref_(~loc), Lident(record2) |> lid(~loc)),
      ),
    );

  let update_field_of_collection =
      (
        ~in_ as record,
        ~collection: Collection.t,
        ~with_ as value,
        ~loc,
        field_name,
      ) =>
    Exp.record(
      [
        (
          Lident(collection.plural) |> lid(~loc),
          Exp.apply(
            [%expr Belt.Array.mapWithIndex],
            [
              (Nolabel, collection.plural |> field(~in_=record, ~loc)),
              (
                Nolabel,
                [%expr
                  (index', item) =>
                    if (index' == index) {
                      %e
                      field_name
                      |> update_field(~in_="item", ~with_=value, ~loc);
                    } else {
                      item;
                    }
                ],
              ),
            ],
          ),
        ),
      ],
      Some(Exp.ident(Lident(record) |> lid(~loc))),
    );

  let update_field_of_collection2 =
      (
        ~in_ as (record1, record2),
        ~collection: Collection.t,
        ~with_ as value,
        ~loc,
        field_name,
      ) =>
    Exp.record(
      [
        (
          Lident(collection.plural) |> lid(~loc),
          Exp.apply(
            [%expr Belt.Array.mapWithIndex],
            [
              (
                Nolabel,
                collection.plural |> field2(~in_=(record1, record2), ~loc),
              ),
              (
                Nolabel,
                [%expr
                  (index', item) =>
                    if (index' == index) {
                      %e
                      field_name
                      |> update_field(~in_="item", ~with_=value, ~loc);
                    } else {
                      item;
                    }
                ],
              ),
            ],
          ),
        ),
      ],
      Some(record2 |> field(~in_=record1, ~loc)),
    );

  let update_ref_field_of_collection =
      (
        ~in_ as record,
        ~collection: Collection.t,
        ~with_ as value,
        ~index_token="index",
        ~loc,
        field_name,
      ) =>
    Exp.record(
      [
        (
          Lident(collection.plural) |> lid(~loc),
          Exp.apply(
            [%expr Belt.Array.mapWithIndex],
            [
              (Nolabel, collection.plural |> ref_field(~in_=record, ~loc)),
              (
                Nolabel,
                [%expr
                  (idx_, item) =>
                    if (idx_
                        == [%e Exp.ident(Lident(index_token) |> lid(~loc))]) {
                      %e
                      field_name
                      |> update_field(~in_="item", ~with_=value, ~loc);
                    } else {
                      item;
                    }
                ],
              ),
            ],
          ),
        ),
      ],
      Some(record |> ref_(~loc)),
    );

  let field_of_collection_validator =
      (~validators, ~collection: Collection.t, ~loc, field) =>
    field |> field3(~in_=(validators, collection.plural, "fields"), ~loc);
};
