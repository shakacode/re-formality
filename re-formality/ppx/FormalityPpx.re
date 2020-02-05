// TODO: PPX collections: `items: [@form.collection] array(item)`
// TODO: PPX deps: `deps: [@form.deps (title, item.name)] string`
// TODO: Whole collection validation
// TODO: Field with deps validation
// TODO: Add/remove items from collections
// TODO: Reorder items in collections

open Ppxlib;
open Ast_helper;

module Error = {
  let report = (~loc, message) => Location.raise_errorf(~loc, message);
};

module Structure = {
  let from_expr = (~loc, expr) =>
    switch (expr) {
    | PStr(structure) => structure
    | _ => Error.report(~loc, "Must be a structure")
    };
};

module StructureItem = {
  let from_type_declaration =
      (~loc: Location.t, ~rec_flag: rec_flag, decl: type_declaration) =>
    Str.type_(~loc, rec_flag, [decl]);
};

module Field = {
  module T: {type t;} = {
    type t = string;
  };

  type t = T.t;
  external to_string: t => string = "%identity";
  external from_string: string => t = "%identity";

  let make = (label: label_declaration) => label.pld_name.txt |> from_string;

  let to_capitalized_string = (field: t) =>
    field |> to_string |> String.capitalize_ascii;

  let eq = (x1, x2) => to_string(x1) == to_string(x2);
  let cmp = (x1, x2) => compare(x1 |> to_string, x2 |> to_string);

  let update_action = x => "Update" ++ (x |> to_capitalized_string) ++ "Field";
  let blur_action = x => "Blur" ++ (x |> to_capitalized_string) ++ "Field";

  let update_fn = x => "update" ++ (x |> to_capitalized_string);
  let blur_fn = x => "blur" ++ (x |> to_capitalized_string);
  let result_fn = x => (x |> to_string) ++ "Result";
};

module FieldType = {
  module T: {type t;} = {
    type t = core_type;
  };

  type t = T.t;

  external make: core_type => t = "%identity";
  let make = (core_type: core_type) => core_type |> make;

  external unpack: t => core_type = "%identity";

  let rec eq = (t1: core_type, t2: core_type) =>
    switch (t1.ptyp_desc, t2.ptyp_desc) {
    | (Ptyp_constr({txt: lid1}, list1), Ptyp_constr({txt: lid2}, list2)) =>
      eq_lid(lid1, lid2) && eq_list(list1, list2)
    | (Ptyp_var(x1), Ptyp_var(x2)) => x1 == x2
    | (Ptyp_tuple(l1), Ptyp_tuple(l2)) => eq_list(l1, l2)
    | _ => false
    }
  and eq_lid = (l1: Longident.t, l2: Longident.t) =>
    switch (l1, l2) {
    | (Lident(x1), Lident(x2)) => x1 == x2
    | (Ldot(l1, x1), Ldot(l2, x2)) => x1 == x2 && eq_lid(l1, l2)
    | (Lapply(l1, l1'), Lapply(l2, l2')) =>
      eq_lid(l1, l2) && eq_lid(l1', l2')
    | _ => false
    }
  and eq_list = (l1: list(core_type), l2: list(core_type)) =>
    if (List.length(l1) == List.length(l2)) {
      List.for_all2((t1, t2) => eq(t1, t2), l1, l2);
    } else {
      false;
    };
  let eq = (x1: t, x2: t) => eq(x1 |> unpack, x2 |> unpack);
};

module FieldDeps = {
  type unvalidated_dep = [ | `Field(string, Location.t)];

  let report_error = (~loc) =>
    Error.report(
      ~loc,
      "[@field.deps] attribute must contain field or tuple of fields",
    );

  let from_attributes = (attributes: list(attribute)) => {
    let deps_attr =
      attributes
      |> List.find_opt(attr =>
           switch (attr) {
           | {attr_name: {txt: "field.deps"}} => true
           | _ => false
           }
         );
    switch (deps_attr) {
    | None => []
    | Some({
        attr_payload: PStr([{pstr_desc: Pstr_eval(exp, _)}]),
        attr_loc,
      }) =>
      let deps: result(list(unvalidated_dep), Location.t) =
        switch (exp) {
        | {pexp_desc: Pexp_ident({txt: Lident(dep), loc})} =>
          Ok([`Field((dep, loc))])
        | {pexp_desc: Pexp_tuple(exps)} =>
          exps
          |> List.fold_left(
               (res, exp) =>
                 switch (res, exp) {
                 | (Error(loc), _) => Error(loc)
                 | (
                     Ok(deps),
                     {pexp_desc: Pexp_ident({txt: Lident(dep), loc})},
                   ) =>
                   Ok([`Field((dep, loc)), ...deps])
                 | (Ok(_), {pexp_loc}) => Error(pexp_loc)
                 },
               Ok([]),
             )
        | {pexp_loc} => Error(pexp_loc)
        };
      switch (deps) {
      | Ok(deps) => deps
      | Error(loc) => report_error(~loc)
      };
    | Some({attr_loc}) => report_error(~loc=attr_loc)
    };
  };
};

module FieldSpec = {
  type t = {
    id: Field.t,
    input_type: FieldType.t,
    output_type: FieldType.t,
    validator: [ | `Required | `Optional],
    deps: list(Field.t),
  };
};

module InputType = {
  module T: {type t;} = {
    type t = structure_item;
  };

  type t = T.t;
  external pack: structure_item => t = "%identity";
  external structure_item: t => structure_item = "%identity";

  let make = (~loc: Location.t, ~rec_flag: rec_flag, decl: type_declaration) =>
    decl |> StructureItem.from_type_declaration(~loc, ~rec_flag) |> pack;
};

module OutputType = {
  module T: {type t;} = {
    type t = structure_item;
  };

  type t = T.t;
  external pack: structure_item => t = "%identity";
  external structure_item: t => structure_item = "%identity";

  let make = (~loc: Location.t, ~rec_flag: rec_flag, decl: type_declaration) =>
    decl |> StructureItem.from_type_declaration(~loc, ~rec_flag) |> pack;
};

module MessageType = {
  module T: {type t;} = {
    type t = structure_item;
  };

  type t = T.t;
  external pack: structure_item => t = "%identity";
  external structure_item: t => structure_item = "%identity";

  let make = (~loc: Location.t, ~rec_flag: rec_flag, decl: type_declaration) =>
    decl |> StructureItem.from_type_declaration(~loc, ~rec_flag) |> pack;

  let default = (~loc) => [%stri type message = string] |> pack;
};

module SubmissionErrorType = {
  module T: {type t;} = {
    type t = structure_item;
  };

  type t = T.t;
  external pack: structure_item => t = "%identity";
  external structure_item: t => structure_item = "%identity";

  let make = (~loc: Location.t, ~rec_flag: rec_flag, decl: type_declaration) =>
    decl |> StructureItem.from_type_declaration(~loc, ~rec_flag) |> pack;

  let default = (~loc) => [%stri type submissionError = unit] |> pack;
};

module InputTypeParser = {
  type result = Pervasives.result(ok, error)
  and ok = {
    fields: list((Field.t, FieldType.t, list(FieldDeps.unvalidated_dep))),
    structure_item: InputType.t,
  }
  and error =
    | NotFound
    | NotRecord(Location.t);
};

module OutputTypeParser = {
  type result = Pervasives.result(ok, error)
  and ok =
    | AliasOfInput(OutputType.t)
    | Record({
        fields: list((Field.t, FieldType.t, Location.t)),
        structure_item: OutputType.t,
        loc: Location.t,
      })
  and error =
    | NotFound
    | NotRecord(Location.t)
    | BadTypeAlias({
        alias: string,
        loc: Location.t,
      });
};

module Config = {
  type data = {
    fields: list(FieldSpec.t),
    input_type: InputType.t,
    output_type: OutputType.t,
    message_type: MessageType.t,
    submission_error_type: SubmissionErrorType.t,
  };

  type error =
    | InputTypeParseError(InputTypeParser.error)
    | OutputTypeParseError(OutputTypeParser.error)
    | IOMismatch(mismatch)
  and mismatch =
    | InputFieldsNotInOutput({
        fields: list(Field.t),
        loc: Location.t,
      })
    | OutputFieldsNotInInput({fields: list((Field.t, Location.t))})
    | Both({
        input_fields_not_in_output: list(Field.t),
        output_fields_not_in_input: list((Field.t, Location.t)),
        loc: Location.t,
      });

  let make = (structure: structure) => {
    let input_parsing_result: ref(option(InputTypeParser.result)) =
      ref(None);
    let output_parsing_result: ref(option(OutputTypeParser.result)) =
      ref(None);
    let message_type: ref(option(MessageType.t)) = ref(None);
    let submission_error_type: ref(option(SubmissionErrorType.t)) =
      ref(None);

    structure
    |> List.iter(
         fun
         | {pstr_desc: Pstr_type(rec_flag, decls)} => {
             decls
             |> List.iter(
                  fun
                  // Input type
                  | {
                      ptype_name: {txt: "input"},
                      ptype_kind: Ptype_record(fields),
                      ptype_loc,
                    } as decl =>
                    input_parsing_result :=
                      Some(
                        Ok({
                          fields:
                            List.fold_right(
                              (field, acc) =>
                                [
                                  (
                                    field |> Field.make,
                                    field.pld_type |> FieldType.make,
                                    field.pld_type.ptyp_attributes
                                    |> FieldDeps.from_attributes,
                                  ),
                                  ...acc,
                                ],
                              fields,
                              [],
                            ),
                          structure_item:
                            decl |> InputType.make(~loc=ptype_loc, ~rec_flag),
                        }),
                      )
                  | {ptype_name: {txt: "input"}, ptype_loc} =>
                    input_parsing_result :=
                      Some(Error(InputTypeParser.NotRecord(ptype_loc)))

                  // Output type
                  | {
                      ptype_name: {txt: "output"},
                      ptype_kind: Ptype_record(fields),
                      ptype_loc,
                    } as decl =>
                    output_parsing_result :=
                      Some(
                        Ok(
                          Record({
                            fields:
                              List.fold_right(
                                (field, acc) =>
                                  [
                                    (
                                      field |> Field.make,
                                      field.pld_type |> FieldType.make,
                                      field.pld_loc,
                                    ),
                                    ...acc,
                                  ],
                                fields,
                                [],
                              ),
                            loc: ptype_loc,
                            structure_item:
                              decl
                              |> OutputType.make(~loc=ptype_loc, ~rec_flag),
                          }),
                        ),
                      )
                  | {
                      ptype_name: {txt: "output"},
                      ptype_kind: Ptype_abstract,
                      ptype_loc,
                      ptype_manifest:
                        Some({
                          ptyp_desc:
                            Ptyp_constr({txt: Lident("input")}, []),
                        }),
                    } as decl =>
                    output_parsing_result :=
                      Some(
                        Ok(
                          AliasOfInput(
                            decl |> OutputType.make(~loc=ptype_loc, ~rec_flag),
                          ),
                        ),
                      )
                  | {
                      ptype_name: {txt: "output"},
                      ptype_kind: Ptype_abstract,
                      ptype_manifest:
                        Some({
                          ptyp_desc:
                            Ptyp_constr({txt: Lident(alias), loc}, []),
                        }),
                    } =>
                    output_parsing_result :=
                      Some(
                        Error(OutputTypeParser.BadTypeAlias({alias, loc})),
                      )
                  | {ptype_name: {txt: "output"}, ptype_loc} =>
                    output_parsing_result :=
                      Some(Error(OutputTypeParser.NotRecord(ptype_loc)))

                  // Message type
                  | {
                      ptype_name: {txt: "message"},
                      ptype_loc,
                      ptype_manifest: Some(_),
                    } as decl =>
                    message_type :=
                      Some(
                        decl |> MessageType.make(~rec_flag, ~loc=ptype_loc),
                      )

                  // Submission error type
                  | {
                      ptype_name: {txt: "submissionError"},
                      ptype_loc,
                      ptype_manifest: Some(_),
                    } as decl =>
                    submission_error_type :=
                      Some(
                        decl
                        |> SubmissionErrorType.make(~rec_flag, ~loc=ptype_loc),
                      )

                  // Rest
                  | _ => (),
                );
           }
         | _ => (),
       );

    switch (input_parsing_result^, output_parsing_result^) {
    | (Some(Error(error)), _) => Error(InputTypeParseError(error))
    | (None, _) => Error(InputTypeParseError(NotFound))
    | (_, Some(Error(error))) => Error(OutputTypeParseError(error))
    | (_, None) => Error(OutputTypeParseError(NotFound))
    | (Some(Ok(input_data)), Some(Ok(output_result))) =>
      let deps_validity =
        input_data.fields
        |> List.fold_left(
             (res, (field, _, deps)) =>
               switch (res) {
               | Error(error) => Error(error)
               | Ok(_) =>
                 deps
                 |> List.fold_left(
                      (res, dep) =>
                        switch (res, dep) {
                        | (Error(error), _) => Error(error)
                        | (Ok (), `Field(dep, loc)) =>
                          switch (
                            deps
                            |> List.find_all(dep' =>
                                 switch (dep') {
                                 | `Field(dep', _) => dep' == dep
                                 }
                               )
                            |> List.length
                          ) {
                          | 0
                          | 1 =>
                            switch (
                              input_data.fields
                              |> List.find_opt(((field, _, _)) =>
                                   field |> Field.to_string == dep
                                 )
                            ) {
                            | None => Error(`DepNotFound((dep, loc)))
                            | Some(_) when dep == (field |> Field.to_string) =>
                              Error(`DepOfItself((dep, loc)))
                            | Some(_) => Ok()
                            }
                          | _ => Error(`DuplicateDep((dep, loc)))
                          }
                        },
                      Ok(),
                    )
               },
             Ok(),
           );
      switch (deps_validity) {
      | Error(`DepNotFound(dep, loc)) =>
        Error.report(~loc, "Field %s doesn't exist in input", dep)
      | Error(`DepOfItself(dep, loc)) =>
        Error.report(~loc, "Field can't depend on itself")
      | Error(`DuplicateDep(dep, loc)) =>
        Error.report(
          ~loc,
          "Field %s is already declared as a dependency for this field",
          dep,
        )
      | Ok () =>
        let fields =
          switch (output_result) {
          | AliasOfInput(_) =>
            Ok(
              input_data.fields
              |> List.map(((field, input_type, deps)) =>
                   FieldSpec.{
                     id: field,
                     input_type,
                     output_type: input_type,
                     validator: `Optional,
                     deps:
                       deps
                       |> List.map(
                            fun
                            | `Field(dep, _) => dep |> Field.from_string,
                          ),
                   }
                 ),
            )
          | Record({fields: output_fields, loc: output_loc}) =>
            let (
              matched_fields,
              input_fields_not_in_output,
              output_fields_not_in_input,
            ) =
              List.fold_right(
                (
                  (input_field, input_field_type, input_field_deps),
                  (
                    matched_fields,
                    input_fields_not_in_output,
                    output_fields_not_in_input,
                  ),
                ) => {
                  let output_field =
                    output_fields
                    |> List.find_opt(((output_field, _, _)) =>
                         input_field |> Field.eq(output_field)
                       );
                  switch (output_field) {
                  | None => (
                      matched_fields,
                      [input_field, ...input_fields_not_in_output],
                      output_fields_not_in_input,
                    )
                  | Some((output_field, output_field_type, _)) => (
                      [
                        FieldSpec.{
                          id: input_field,
                          input_type: input_field_type,
                          output_type: output_field_type,
                          validator:
                            if (FieldType.eq(
                                  input_field_type,
                                  output_field_type,
                                )) {
                              `Optional;
                            } else {
                              `Required;
                            },
                          deps:
                            input_field_deps
                            |> List.map(
                                 fun
                                 | `Field(dep, _) => dep |> Field.from_string,
                               ),
                        },
                        ...matched_fields,
                      ],
                      input_fields_not_in_output,
                      output_fields_not_in_input
                      |> List.filter(((output_field, _, _)) =>
                           !(input_field |> Field.eq(output_field))
                         ),
                    )
                  };
                },
                input_data.fields,
                ([], [], output_fields),
              );
            switch (input_fields_not_in_output, output_fields_not_in_input) {
            | ([], []) => Ok(matched_fields)
            | (input_fields_not_in_output, []) =>
              Error(
                IOMismatch(
                  InputFieldsNotInOutput({
                    fields: input_fields_not_in_output,
                    loc: output_loc,
                  }),
                ),
              )
            | ([], output_fields_not_in_input) =>
              Error(
                IOMismatch(
                  OutputFieldsNotInInput({
                    fields:
                      output_fields_not_in_input
                      |> List.map(((field, _, loc)) => (field, loc)),
                  }),
                ),
              )
            | (input_fields_not_in_output, output_fields_not_in_input) =>
              Error(
                IOMismatch(
                  Both({
                    input_fields_not_in_output,
                    output_fields_not_in_input:
                      output_fields_not_in_input
                      |> List.map(((field, _, loc)) => (field, loc)),
                    loc: output_loc,
                  }),
                ),
              )
            };
          };

        switch (fields) {
        | Ok(fields) =>
          Ok({
            fields,
            input_type: input_data.structure_item,
            output_type:
              switch (output_result) {
              | AliasOfInput(structure_item)
              | Record({structure_item}) => structure_item
              },
            message_type:
              switch (message_type^) {
              | Some(x) => x
              | None => MessageType.default(~loc=Location.none)
              },
            submission_error_type:
              switch (submission_error_type^) {
              | Some(x) => x
              | None => SubmissionErrorType.default(~loc=Location.none)
              },
          })
        | Error(error) => Error(error)
        };
      };
    };
  };

  let unwrap = (result, ~loc): data =>
    switch (result) {
    | Ok(data) => data
    | Error(InputTypeParseError(NotFound)) =>
      Error.report(~loc, "`input` type not found")
    | Error(InputTypeParseError(NotRecord(loc))) =>
      Error.report(~loc, "`input` must be of record type")
    | Error(OutputTypeParseError(NotFound)) =>
      Error.report(~loc, "`output` type not found")
    | Error(OutputTypeParseError(NotRecord(loc))) =>
      Error.report(
        ~loc,
        "`output` must be of record type or an alias of `input`",
      )
    | Error(OutputTypeParseError(BadTypeAlias({alias, loc}))) =>
      Error.report(
        ~loc,
        "`output` can only be an alias of `input` type or a record",
      )
    | Error(IOMismatch(OutputFieldsNotInInput({fields}))) =>
      switch (fields) {
      | [] =>
        failwith(
          "Empty list of non-matched fields in IOMatchError(OutputFieldsNotInInput)",
        )
      | [(field, loc)]
      | [(field, loc), ..._] =>
        Error.report(
          ~loc,
          "`output` field `%s` doesn't exist in `input` type",
          field |> Field.to_string,
        )
      }
    | Error(IOMismatch(InputFieldsNotInOutput({fields, loc})))
    | Error(
        IOMismatch(
          Both({
            input_fields_not_in_output: fields,
            output_fields_not_in_input: _,
            loc,
          }),
        ),
      ) =>
      switch (fields) {
      | [] =>
        failwith(
          "Empty list of non-matched fields in IOMatchError(TotalMess)",
        )
      | [field] =>
        Error.report(
          ~loc,
          "`input` field `%s` doesn't exist in `output` type",
          field |> Field.to_string,
        )
      | fields =>
        Error.report(
          ~loc,
          "Some `input` fields don't exist in `output` type: %s",
          fields |> List.map(Field.to_string) |> String.concat(", "),
        )
      }
    };
};

module AstHelpers = {
  let lid = (~loc, x: Longident.t) => {txt: x, loc};
  let str = (~loc, x: string) => {txt: x, loc};

  let explicit_arity = (~loc) => {
    attr_name: "explicit_arity" |> str(~loc),
    attr_payload: PStr([]),
    attr_loc: Location.none,
  };

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
          ~typ: FieldSpec.t => core_type,
          fields: list(FieldSpec.t),
        ) =>
      name
      |> str(~loc)
      |> Type.mk(
           ~kind=
             Ptype_record(
               fields
               |> List.map((field: FieldSpec.t) =>
                    Type.field(
                      field.id |> Field.to_string |> str(~loc),
                      field |> typ,
                    )
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
        Lident(field |> Field.to_string) |> lid(~loc),
      );

    let field2 = (~of_ as (record1, record2), ~loc, field: Field.t) =>
      Exp.field(
        Exp.field(
          Exp.ident(Lident(record1) |> lid(~loc)),
          Lident(record2) |> lid(~loc),
        ),
        Lident(field |> Field.to_string) |> lid(~loc),
      );

    let ref_field = (~of_ as record, ~loc, field: Field.t) =>
      Exp.field(
        record |> ref_(~loc),
        Lident(field |> Field.to_string) |> lid(~loc),
      );

    let update_field = (~of_ as record, ~with_ as value, ~loc, field: Field.t) =>
      Exp.record(
        [(Lident(field |> Field.to_string) |> lid(~loc), value)],
        Some(Exp.ident(Lident(record) |> lid(~loc))),
      );

    let update_ref_field =
        (~of_ as record, ~with_ as value, ~loc, field: Field.t) =>
      Exp.record(
        [(Lident(field |> Field.to_string) |> lid(~loc), value)],
        Some(record |> ref_(~loc)),
      );

    let record = (~loc, xs: list((string, expression))) =>
      Exp.record(
        xs
        |> List.map(((name, expr)) => (Lident(name) |> lid(~loc), expr)),
        None,
      );
  };
};

module Render = {
  open AstHelpers;

  let module_form = (~loc) => {
    [%stri module Form = Formality__Form];
  };

  let module_validation = (~loc) => {
    [%stri module Validation = Formality__Validation];
  };

  let module_strategy = (~loc) => {
    [%stri module Strategy = Formality__Strategy];
  };

  let module_form_status = (~loc) => {
    [%stri module FormStatus = Formality__FormStatus];
  };

  let module_react_update = (~loc) => {
    [%stri module ReactUpdate = Formality__ReactUpdate];
  };

  let input_type = (input_type: InputType.t) => {
    input_type |> InputType.structure_item;
  };

  let output_type = (output_type: OutputType.t) => {
    output_type |> OutputType.structure_item;
  };

  let message_type = (message_type: MessageType.t) =>
    message_type |> MessageType.structure_item;

  let submission_error_type = (submission_error_type: SubmissionErrorType.t) =>
    submission_error_type |> SubmissionErrorType.structure_item;

  let validators_type = (~loc, fields: list(FieldSpec.t)) => {
    fields
    |> T.record_of_fields(
         ~name="validators",
         ~loc,
         ~typ=field => {
           let typ =
             Typ.constr(
               Ldot(Ldot(Lident("Validation"), "SingleValue"), "validator")
               |> lid(~loc),
               [
                 Typ.constr(Lident("input") |> lid(~loc), []),
                 field.output_type |> FieldType.unpack,
                 Typ.constr(Lident("message") |> lid(~loc), []),
               ],
             );
           switch (field.validator) {
           | `Required => typ
           | `Optional => Typ.constr(Lident("option") |> lid(~loc), [typ])
           };
         },
       );
  };

  let fields_statuses_type = (~loc, fields: list(FieldSpec.t)) => {
    fields
    |> T.record_of_fields(~name="fieldsStatuses", ~loc, ~typ=field =>
         Typ.constr(
           Ldot(Lident("Validation"), "status") |> lid(~loc),
           [
             field.output_type |> FieldType.unpack,
             Typ.constr(Lident("message") |> lid(~loc), []),
           ],
         )
       );
  };

  let state_type = (~loc) => [%stri
    type state = {
      input,
      fieldsStatuses,
      formStatus: FormStatus.t(submissionError),
      formSubmissions: FormStatus.submission,
    }
  ];

  let action_type = (~loc, fields: list(FieldSpec.t)) => {
    let update_actions =
      fields
      |> List.map((field: FieldSpec.t) =>
           field.id
           |> Field.update_action
           |> T.constructor(~args=[[%type: input]], ~loc)
         );
    let blur_actions =
      fields
      |> List.map((field: FieldSpec.t) =>
           field.id |> Field.blur_action |> T.constructor(~loc)
         );
    let rest_actions = [
      "Submit" |> T.constructor(~loc),
      "SetSubmittedStatus"
      |> T.constructor(~args=[[%type: option(input)]], ~loc),
      "SetSubmissionFailedStatus"
      |> T.constructor(~args=[[%type: submissionError]], ~loc),
      "MapSubmissionError"
      |> T.constructor(
           ~args=[[%type: submissionError => submissionError]],
           ~loc,
         ),
      "DismissSubmissionError" |> T.constructor(~loc),
      "DismissSubmissionResult" |> T.constructor(~loc),
      "Reset" |> T.constructor(~loc),
    ];

    "action"
    |> str(~loc)
    |> Type.mk(
         ~kind=
           Ptype_variant(
             rest_actions
             |> List.append(blur_actions)
             |> List.append(update_actions),
           ),
       )
    |> StructureItem.from_type_declaration(~loc, ~rec_flag=Recursive);
  };

  let interface_type = (~loc, fields: list(FieldSpec.t)) => {
    let f = (x, t) => t |> Type.field(x |> str(~loc));

    let base = [
      f("input", [%type: input]),
      f("status", [%type: FormStatus.t(submissionError)]),
      f("dirty", [%type: unit => bool]),
      f("valid", [%type: unit => bool]),
      f("submitting", [%type: bool]),
      f("submit", [%type: unit => unit]),
      f("dismissSubmissionError", [%type: unit => unit]),
      f("dismissSubmissionResult", [%type: unit => unit]),
      f(
        "mapSubmissionError",
        [%type: (submissionError => submissionError) => unit],
      ),
      f("reset", [%type: unit => unit]),
    ];

    let update_fns =
      fields
      |> List.map((field: FieldSpec.t) => {
           f(field.id |> Field.update_fn, [%type: input => unit])
         });

    let blur_fns =
      fields
      |> List.map((field: FieldSpec.t) => {
           f(field.id |> Field.blur_fn, [%type: unit => unit])
         });

    let result_fns =
      fields
      |> List.map((field: FieldSpec.t) => {
           f(
             field.id |> Field.result_fn,
             [%type:
               unit =>
               option(
                 result([%t field.output_type |> FieldType.unpack], message),
               )
             ],
           )
         });

    "interface"
    |> str(~loc)
    |> Type.mk(
         ~kind=
           Ptype_record(
             base
             |> List.append(result_fns)
             |> List.append(blur_fns)
             |> List.append(update_fns),
           ),
       )
    |> StructureItem.from_type_declaration(~loc, ~rec_flag=Recursive);
  };

  let initial_fields_statuses_fn = (~loc, fields: list(FieldSpec.t)) => {
    [%stri
      let initialFieldsStatuses = (_input: input) => [%e
        Exp.record(
          fields
          |> List.map((field: FieldSpec.t) =>
               (
                 Lident(field.id |> Field.to_string) |> lid(~loc),
                 [%expr Pristine],
               )
             ),
          None,
        )
      ]
    ];
  };

  let initial_state_fn = (~loc) => [%stri
    let initialState = input => {
      input,
      fieldsStatuses: input->initialFieldsStatuses,
      formStatus: Editing,
      formSubmissions: NeverSubmitted,
    }
  ];

  let validate_form_fn = (~loc, fields: list(FieldSpec.t)) => {
    let field_result = x => (x |> Field.to_string) ++ "Result";
    let field_result_visibility = x =>
      (x |> Field.to_string) ++ "ResultVisibility";

    [%stri
      let validateForm =
          (input: input, ~validators: validators)
          : Validation.result(output, fieldsStatuses) => [%e
        Exp.match(
          Exp.tuple(
            fields
            |> List.map((field: FieldSpec.t) =>
                 switch (field.validator) {
                 | `Required =>
                   %expr
                   {
                     let Validation.SingleValue.{validate} = [%e
                       field.id |> E.field(~of_="validators", ~loc)
                     ];
                     (input->validate, Validation.Visibility.Shown);
                   }
                 | `Optional =>
                   switch%expr (
                     [%e field.id |> E.field(~of_="validators", ~loc)]
                   ) {
                   | Some({validate}) => (
                       input->validate,
                       Validation.Visibility.Shown,
                     )
                   | None => (
                       Ok([%e field.id |> E.field(~of_="input", ~loc)]),
                       Validation.Visibility.Hidden,
                     )
                   }
                 }
               ),
          ),
          [
            // ((Ok(value), visibility), ...) => Ok(...)
            Exp.case(
              Pat.tuple(
                fields
                |> List.map((field: FieldSpec.t) =>
                     Pat.tuple([
                       Pat.alias(
                         Pat.construct(
                           ~attrs=[explicit_arity(~loc)],
                           Lident("Ok") |> lid(~loc),
                           Some(
                             Pat.tuple([
                               Pat.var(
                                 field.id |> Field.to_string |> str(~loc),
                               ),
                             ]),
                           ),
                         ),
                         field.id |> field_result |> str(~loc),
                       ),
                       Pat.var(
                         field.id |> field_result_visibility |> str(~loc),
                       ),
                     ])
                   ),
              ),
              [%expr
                Ok({
                  output: [%e
                    Exp.record(
                      fields
                      |> List.map((field: FieldSpec.t) =>
                           (
                             Lident(field.id |> Field.to_string) |> lid(~loc),
                             Exp.ident(
                               Lident(field.id |> Field.to_string)
                               |> lid(~loc),
                             ),
                           )
                         ),
                      None,
                    )
                  ],
                  fieldsStatuses: [%e
                    Exp.record(
                      fields
                      |> List.map((field: FieldSpec.t) =>
                           (
                             Lident(field.id |> Field.to_string) |> lid(~loc),
                             [%expr
                               Dirty(
                                 [%e
                                   Exp.ident(
                                     Lident(field.id |> field_result)
                                     |> lid(~loc),
                                   )
                                 ],
                                 [%e
                                   Exp.ident(
                                     Lident(
                                       field.id |> field_result_visibility,
                                     )
                                     |> lid(~loc),
                                   )
                                 ],
                               )
                             ],
                           )
                         ),
                      None,
                    )
                  ],
                })
              ],
            ),
            // ((_, visibility), ...) => Error(...)
            Exp.case(
              Pat.tuple(
                fields
                |> List.map((field: FieldSpec.t) =>
                     Pat.tuple([
                       Pat.var(field.id |> field_result |> str(~loc)),
                       Pat.var(
                         field.id |> field_result_visibility |> str(~loc),
                       ),
                     ])
                   ),
              ),
              [%expr
                Error({
                  fieldsStatuses: [%e
                    Exp.record(
                      fields
                      |> List.map((field: FieldSpec.t) =>
                           (
                             Lident(field.id |> Field.to_string) |> lid(~loc),
                             [%expr
                               Dirty(
                                 [%e
                                   Exp.ident(
                                     Lident(field.id |> field_result)
                                     |> lid(~loc),
                                   )
                                 ],
                                 [%e
                                   Exp.ident(
                                     Lident(
                                       field.id |> field_result_visibility,
                                     )
                                     |> lid(~loc),
                                   )
                                 ],
                               )
                             ],
                           )
                         ),
                      None,
                    )
                  ],
                })
              ],
            ),
          ],
        )
      ]
    ];
  };

  let use_form_fn = (~loc, fields: list(FieldSpec.t)) => [%stri
    let useForm =
        (
          ~initialInput: input,
          ~validators: validators,
          ~onSubmit:
             (
               output,
               Validation.submissionCallbacks(input, submissionError)
             ) =>
             unit,
        ) => {
      // Reducer
      let memoizedInitialState =
        React.useMemo1(() => initialInput->initialState, [|initialInput|]);

      let (state, dispatch) =
        memoizedInitialState->ReactUpdate.useReducer((state, action) => {
          %e
          {
            let update_actions =
              fields
              |> List.map((field: FieldSpec.t) =>
                   Exp.case(
                     Pat.construct(
                       ~attrs=[explicit_arity(~loc)],
                       Lident(field.id |> Field.update_action) |> lid(~loc),
                       Some(Pat.tuple([Pat.var("input" |> str(~loc))])),
                     ),
                     switch (field.deps) {
                     | [] =>
                       %expr
                       {
                         let {fieldsStatuses, formSubmissions} = state;
                         Update({
                           ...state,
                           input,
                           fieldsStatuses:
                             switch%e (field.validator) {
                             | `Required =>
                               %expr
                               {
                                 Form.validateFieldOnChangeWithValidator(
                                   ~input,
                                   ~status=[%e
                                     field.id
                                     |> E.field(~of_="fieldsStatuses", ~loc)
                                   ],
                                   ~submission=formSubmissions,
                                   ~validator=[%e
                                     field.id
                                     |> E.field(~of_="validators", ~loc)
                                   ],
                                   ~setStatus=[%e
                                     [%expr
                                       status => [%e
                                         field.id
                                         |> E.update_field(
                                              ~of_="fieldsStatuses",
                                              ~with_=[%expr status],
                                              ~loc,
                                            )
                                       ]
                                     ]
                                   ],
                                 );
                               }
                             | `Optional =>
                               switch%expr (
                                 [%e
                                   field.id
                                   |> E.field(~of_="validators", ~loc)
                                 ]
                               ) {
                               | Some(validator) =>
                                 Form.validateFieldOnChangeWithValidator(
                                   ~input,
                                   ~status=[%e
                                     field.id
                                     |> E.field(~of_="fieldsStatuses", ~loc)
                                   ],
                                   ~submission=formSubmissions,
                                   ~validator,
                                   ~setStatus=[%e
                                     [%expr
                                       status => [%e
                                         field.id
                                         |> E.update_field(
                                              ~of_="fieldsStatuses",
                                              ~with_=[%expr status],
                                              ~loc,
                                            )
                                       ]
                                     ]
                                   ],
                                 )
                               | None =>
                                 Form.validateFieldOnChangeWithoutValidator(
                                   ~fieldInput=[%e
                                     field.id |> E.field(~of_="input", ~loc)
                                   ],
                                   ~setStatus=[%e
                                     [%expr
                                       status => [%e
                                         field.id
                                         |> E.update_field(
                                              ~of_="fieldsStatuses",
                                              ~with_=[%expr status],
                                              ~loc,
                                            )
                                       ]
                                     ]
                                   ],
                                 )
                               }
                             },
                         });
                       }
                     | [dep, ...deps] =>
                       %expr
                       {
                         let fieldsStatuses = ref(state.fieldsStatuses);
                         let {formSubmissions} = state;

                         %e
                         {
                           let validate_dep = dep => {
                             let field =
                               fields
                               |> List.find((field: FieldSpec.t) =>
                                    field.id |> Field.eq(dep)
                                  );
                             switch (field.validator) {
                             | `Required =>
                               switch%expr (
                                 Form.validateFieldDependencyOnChange(
                                   ~input,
                                   ~status=[%e
                                     field.id
                                     |> E.ref_field(
                                          ~of_="fieldsStatuses",
                                          ~loc,
                                        )
                                   ],
                                   ~validator=[%e
                                     field.id
                                     |> E.field(~of_="validators", ~loc)
                                   ],
                                   ~setStatus=[%e
                                     [%expr
                                       status => [%e
                                         field.id
                                         |> E.update_ref_field(
                                              ~of_="fieldsStatuses",
                                              ~with_=[%expr status],
                                              ~loc,
                                            )
                                       ]
                                     ]
                                   ],
                                 )
                               ) {
                               | Some(result) => fieldsStatuses := result
                               | None => ()
                               }
                             | `Optional =>
                               switch%expr (
                                 [%e
                                   field.id
                                   |> E.field(~of_="validators", ~loc)
                                 ]
                               ) {
                               | None => ()
                               | Some(validator) =>
                                 switch (
                                   Form.validateFieldDependencyOnChange(
                                     ~input,
                                     ~status=[%e
                                       field.id
                                       |> E.ref_field(
                                            ~of_="fieldsStatuses",
                                            ~loc,
                                          )
                                     ],
                                     ~validator,
                                     ~setStatus=[%e
                                       [%expr
                                         status => [%e
                                           field.id
                                           |> E.update_ref_field(
                                                ~of_="fieldsStatuses",
                                                ~with_=[%expr status],
                                                ~loc,
                                              )
                                         ]
                                       ]
                                     ],
                                   )
                                 ) {
                                 | Some(result) => fieldsStatuses := result
                                 | None => ()
                                 }
                               }
                             };
                           };
                           deps
                           |> E.seq(
                                ~exp=dep |> validate_dep,
                                ~make=validate_dep,
                              );
                         };

                         Update({
                           ...state,
                           input,
                           fieldsStatuses:
                             switch%e (field.validator) {
                             | `Required =>
                               %expr
                               {
                                 Form.validateFieldOnChangeWithValidator(
                                   ~input,
                                   ~status=[%e
                                     field.id
                                     |> E.ref_field(
                                          ~of_="fieldsStatuses",
                                          ~loc,
                                        )
                                   ],
                                   ~submission=formSubmissions,
                                   ~validator=[%e
                                     field.id
                                     |> E.field(~of_="validators", ~loc)
                                   ],
                                   ~setStatus=[%e
                                     [%expr
                                       status => [%e
                                         field.id
                                         |> E.update_ref_field(
                                              ~of_="fieldsStatuses",
                                              ~with_=[%expr status],
                                              ~loc,
                                            )
                                       ]
                                     ]
                                   ],
                                 );
                               }
                             | `Optional =>
                               switch%expr (
                                 [%e
                                   field.id
                                   |> E.field(~of_="validators", ~loc)
                                 ]
                               ) {
                               | Some(validator) =>
                                 Form.validateFieldOnChangeWithValidator(
                                   ~input,
                                   ~status=[%e
                                     field.id
                                     |> E.ref_field(
                                          ~of_="fieldsStatuses",
                                          ~loc,
                                        )
                                   ],
                                   ~submission=formSubmissions,
                                   ~validator,
                                   ~setStatus=[%e
                                     [%expr
                                       status => [%e
                                         field.id
                                         |> E.update_ref_field(
                                              ~of_="fieldsStatuses",
                                              ~with_=[%expr status],
                                              ~loc,
                                            )
                                       ]
                                     ]
                                   ],
                                 )
                               | None =>
                                 Form.validateFieldOnChangeWithoutValidator(
                                   ~fieldInput=[%e
                                     field.id |> E.field(~of_="input", ~loc)
                                   ],
                                   ~setStatus=[%e
                                     [%expr
                                       status => [%e
                                         field.id
                                         |> E.update_ref_field(
                                              ~of_="fieldsStatuses",
                                              ~with_=[%expr status],
                                              ~loc,
                                            )
                                       ]
                                     ]
                                   ],
                                 )
                               }
                             },
                         });
                       }
                     },
                   )
                 );

            let blur_actions =
              fields
              |> List.map((field: FieldSpec.t) =>
                   Exp.case(
                     Pat.construct(
                       Lident(field.id |> Field.blur_action) |> lid(~loc),
                       None,
                     ),
                     {
                       %expr
                       {
                         let {input, fieldsStatuses} = state;
                         let result =
                           switch%e (field.validator) {
                           | `Required =>
                             %expr
                             Form.validateFieldOnBlurWithValidator(
                               ~input,
                               ~status=[%e
                                 field.id
                                 |> E.field(~of_="fieldsStatuses", ~loc)
                               ],
                               ~validator=[%e
                                 field.id |> E.field(~of_="validators", ~loc)
                               ],
                               ~setStatus=[%e
                                 [%expr
                                   status => [%e
                                     field.id
                                     |> E.update_field(
                                          ~of_="fieldsStatuses",
                                          ~with_=[%expr status],
                                          ~loc,
                                        )
                                   ]
                                 ]
                               ],
                             )
                           | `Optional =>
                             switch%expr (
                               [%e
                                 field.id |> E.field(~of_="validators", ~loc)
                               ]
                             ) {
                             | Some(validator) =>
                               Form.validateFieldOnBlurWithValidator(
                                 ~input,
                                 ~status=[%e
                                   field.id
                                   |> E.field(~of_="fieldsStatuses", ~loc)
                                 ],
                                 ~validator,
                                 ~setStatus=[%e
                                   [%expr
                                     status => [%e
                                       field.id
                                       |> E.update_field(
                                            ~of_="fieldsStatuses",
                                            ~with_=[%expr status],
                                            ~loc,
                                          )
                                     ]
                                   ]
                                 ],
                               )
                             | None =>
                               Form.validateFieldOnBlurWithoutValidator(
                                 ~fieldInput=[%e
                                   field.id |> E.field(~of_="input", ~loc)
                                 ],
                                 ~status=[%e
                                   field.id
                                   |> E.field(~of_="fieldsStatuses", ~loc)
                                 ],
                                 ~setStatus=[%e
                                   [%expr
                                     status => [%e
                                       field.id
                                       |> E.update_field(
                                            ~of_="fieldsStatuses",
                                            ~with_=[%expr status],
                                            ~loc,
                                          )
                                     ]
                                   ]
                                 ],
                               )
                             }
                           };
                         switch (result) {
                         | Some(fieldsStatuses) =>
                           Update({...state, fieldsStatuses})
                         | None => NoUpdate
                         };
                       };
                     },
                   )
                 );
            let rest_actions = [
              Exp.case(
                [%pat? Submit],
                switch%expr (state.formStatus) {
                | Submitting(_) => NoUpdate
                | Editing
                | Submitted
                | SubmissionFailed(_) =>
                  switch (state.input->validateForm(~validators)) {
                  | Ok({output, fieldsStatuses}) =>
                    UpdateWithSideEffects(
                      {
                        ...state,
                        fieldsStatuses,
                        formStatus:
                          FormStatus.Submitting(
                            switch (state.formStatus) {
                            | SubmissionFailed(error) => Some(error)
                            | Editing
                            | Submitted
                            | Submitting(_) => None
                            },
                          ),
                        formSubmissions: AttemptedToSubmit,
                      },
                      ({dispatch}) =>
                        output->onSubmit({
                          notifyOnSuccess: input =>
                            SetSubmittedStatus(input)->dispatch,
                          notifyOnFailure: error =>
                            SetSubmissionFailedStatus(error)->dispatch,
                          reset: () => Reset->dispatch,
                          dismissSubmissionResult: () =>
                            DismissSubmissionResult->dispatch,
                        }),
                    )
                  | Error({fieldsStatuses}) =>
                    Update({
                      ...state,
                      fieldsStatuses,
                      formStatus: Editing,
                      formSubmissions: AttemptedToSubmit,
                    })
                  }
                },
              ),
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
                [%expr
                  Update({...state, formStatus: SubmissionFailed(error)})
                ],
              ),
              Exp.case(
                [%pat? MapSubmissionError(map)],
                switch%expr (state.formStatus) {
                | Submitting(Some(error)) =>
                  Update({
                    ...state,
                    formStatus: Submitting(Some(error->map)),
                  })
                | SubmissionFailed(error) =>
                  Update({
                    ...state,
                    formStatus: SubmissionFailed(error->map),
                  })
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
                | SubmissionFailed(_) =>
                  Update({...state, formStatus: Editing})
                },
              ),
              Exp.case(
                [%pat? DismissSubmissionResult],
                switch%expr (state.formStatus) {
                | Editing
                | Submitting(_) => NoUpdate
                | Submitted
                | SubmissionFailed(_) =>
                  Update({...state, formStatus: Editing})
                },
              ),
              Exp.case(
                [%pat? Reset],
                [%expr Update(initialInput->initialState)],
              ),
            ];
            Exp.match(
              [%expr action],
              rest_actions
              |> List.append(blur_actions)
              |> List.append(update_actions),
            );
          }
        });

      // Interface
      %e
      {
        let base = [
          ("input", [%expr state.input]),
          ("status", [%expr state.formStatus]),
          (
            "dirty",
            [%expr
              () => [%e
                Exp.match(
                  [%expr state.fieldsStatuses],
                  [
                    Exp.case(
                      Pat.record(
                        fields
                        |> List.map((field: FieldSpec.t) =>
                             (
                               Lident(field.id |> Field.to_string)
                               |> lid(~loc),
                               [%pat? Pristine],
                             )
                           ),
                        Closed,
                      ),
                      [%expr false],
                    ),
                    Exp.case([%pat? _], [%expr true]),
                  ],
                )
              ]
            ],
          ),
          (
            "valid",
            [%expr
              () =>
                switch (state.input->validateForm(~validators)) {
                | Ok(_) => true
                | Error(_) => false
                }
            ],
          ),
          (
            "submitting",
            switch%expr (state.formStatus) {
            | Submitting(_) => true
            | Editing
            | Submitted
            | SubmissionFailed(_) => false
            },
          ),
          ("submit", [%expr () => Submit->dispatch]),
          (
            "mapSubmissionError",
            [%expr map => MapSubmissionError(map)->dispatch],
          ),
          (
            "dismissSubmissionError",
            [%expr () => DismissSubmissionError->dispatch],
          ),
          (
            "dismissSubmissionResult",
            [%expr () => DismissSubmissionResult->dispatch],
          ),
          ("reset", [%expr () => Reset->dispatch]),
        ];
        let update_fns =
          fields
          |> List.map((field: FieldSpec.t) => {
               (
                 field.id |> Field.update_fn,
                 [%expr
                   input =>
                     [%e
                       Exp.construct(
                         Lident(field.id |> Field.update_action) |> lid(~loc),
                         Some([%expr input]),
                       )
                     ]
                     ->dispatch
                 ],
               )
             });
        let blur_fns =
          fields
          |> List.map((field: FieldSpec.t) => {
               (
                 field.id |> Field.blur_fn,
                 [%expr
                   () =>
                     [%e
                       Exp.construct(
                         Lident(field.id |> Field.blur_action) |> lid(~loc),
                         None,
                       )
                     ]
                     ->dispatch
                 ],
               )
             });
        let result_fns =
          fields
          |> List.map((field: FieldSpec.t) => {
               (
                 field.id |> Field.result_fn,
                 [%expr
                   () => {
                     Form.exposeFieldResult(
                       [%e
                         field.id
                         |> E.field2(~of_=("state", "fieldsStatuses"), ~loc)
                       ],
                     );
                   }
                 ],
               )
             });

        E.record(
          ~loc,
          result_fns
          |> List.append(blur_fns)
          |> List.append(update_fns)
          |> List.append(base),
        );
      };
    }
  ];
};

let ext =
  Extension.declare(
    "form",
    Extension.Context.module_expr,
    Ast_pattern.__,
    (~loc, ~path as _, expr) => {
      let Config.{
            fields,
            input_type,
            output_type,
            message_type,
            submission_error_type,
          } =
        expr
        |> Structure.from_expr(~loc)
        |> Config.make
        |> Config.unwrap(~loc);

      Mod.mk(
        Pmod_structure([
          Render.module_form(~loc),
          Render.module_validation(~loc),
          Render.module_strategy(~loc),
          Render.module_form_status(~loc),
          Render.module_react_update(~loc),
          Render.input_type(input_type),
          Render.output_type(output_type),
          Render.message_type(message_type),
          Render.submission_error_type(submission_error_type),
          Render.validators_type(~loc, fields),
          Render.fields_statuses_type(~loc, fields),
          Render.state_type(~loc),
          Render.action_type(~loc, fields),
          Render.interface_type(~loc, fields),
          Render.initial_fields_statuses_fn(~loc, fields),
          Render.initial_state_fn(~loc),
          Render.validate_form_fn(~loc, fields),
          Render.use_form_fn(~loc, fields),
        ]),
      );
    },
  );

"formality" |> Driver.register_transformation(~extensions=[ext]);
