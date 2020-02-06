open Ast;

open Ppxlib;

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

  type error =
    | DepsParseError(Location.t)
    | DepNotFound(unvalidated_dep)
    | DepOfItself(unvalidated_dep)
    | DepDuplicate(unvalidated_dep);

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
    | None => Ok([])
    | Some({
        attr_payload: PStr([{pstr_desc: Pstr_eval(exp, _)}]),
        attr_loc,
      }) =>
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
               | (Ok(_), {pexp_loc}) => Error(DepsParseError(pexp_loc))
               },
             Ok([]),
           )
      | {pexp_loc} => Error(DepsParseError(pexp_loc))
      }
    | Some({attr_loc}) => Error(DepsParseError(attr_loc))
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
    | NotRecord(Location.t)
    | InvalidFieldDeps(FieldDeps.error);
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

module Data = {
  type t = {
    fields: list(FieldSpec.t),
    input_type: InputType.t,
    output_type: OutputType.t,
    message_type: MessageType.t,
    submission_error_type: SubmissionErrorType.t,
  };

  type error =
    | InputTypeParseError(InputTypeParser.error)
    | OutputTypeParseError(OutputTypeParser.error)
    | IOMismatch(io_mismatch)
  and io_mismatch =
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
                        {
                          let fields =
                            List.fold_right(
                              (field, res) =>
                                switch (
                                  res,
                                  field.pld_type.ptyp_attributes
                                  |> FieldDeps.from_attributes,
                                ) {
                                | (Ok(fields), Ok(deps)) =>
                                  Ok([
                                    (
                                      field |> Field.make,
                                      field.pld_type |> FieldType.make,
                                      deps,
                                    ),
                                    ...fields,
                                  ])
                                | (Error(error), _)
                                | (_, Error(error)) => Error(error)
                                },
                              fields,
                              Ok([]),
                            );
                          switch (fields) {
                          | Error(error) => Error(InvalidFieldDeps(error))
                          | Ok(fields) =>
                            Ok({
                              fields,
                              structure_item:
                                decl
                                |> InputType.make(~loc=ptype_loc, ~rec_flag),
                            })
                          };
                        },
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
                        | (Ok (), `Field(dep_name, loc)) =>
                          switch (
                            deps
                            |> List.find_all(dep' =>
                                 switch (dep') {
                                 | `Field(dep', _) => dep' == dep_name
                                 }
                               )
                            |> List.length
                          ) {
                          | 0
                          | 1 =>
                            switch (
                              input_data.fields
                              |> List.find_opt(((field, _, _)) =>
                                   field |> Field.to_string == dep_name
                                 )
                            ) {
                            | None => Error(FieldDeps.DepNotFound(dep))
                            | Some(_)
                                when dep_name == (field |> Field.to_string) =>
                              Error(FieldDeps.DepOfItself(dep))
                            | Some(_) => Ok()
                            }
                          | _ => Error(FieldDeps.DepDuplicate(dep))
                          }
                        },
                      Ok(),
                    )
               },
             Ok(),
           );
      switch (deps_validity) {
      | Error(error) => Error(InputTypeParseError(InvalidFieldDeps(error)))
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
};
