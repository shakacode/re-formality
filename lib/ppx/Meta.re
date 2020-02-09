open Ast;

open Ppxlib;

module Field = {
  type t =
    | Field(string);

  let make = (label: label_declaration) => Field(label.pld_name.txt);

  let to_camelized_string =
    fun
    | Field(field) => field;

  let to_capitalized_string =
    fun
    | Field(field) => field |> String.capitalize_ascii;

  let eq = (x1, x2) =>
    switch (x1, x2) {
    | (Field(x1), Field(x2)) => x1 == x2
    };

  let cmp = (x1, x2) =>
    switch (x1, x2) {
    | (Field(x1), Field(x2)) => compare(x1, x2)
    };

  let update_action = x => "Update" ++ (x |> to_capitalized_string) ++ "Field";
  let blur_action = x => "Blur" ++ (x |> to_capitalized_string) ++ "Field";
  let apply_async_result_action = x =>
    "ApplyAsyncResultFor" ++ (x |> to_capitalized_string) ++ "Field";

  let update_fn = x => "update" ++ (x |> to_capitalized_string);
  let blur_fn = x => "blur" ++ (x |> to_capitalized_string);
  let result_fn = x => (x |> to_camelized_string) ++ "Result";
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

module FieldOptionality = {
  type t =
    | OptionType
    | StringType
    | OptionStringType;
};

module AsyncMode = {
  type t =
    | OnChange
    | OnBlur;

  let default = OnChange;
};

module ValidatorsRecord = {
  type t = {
    fields,
    rec_flag,
    constraint_metadata: metadata,
    record_metadata: metadata,
    annotation: core_type,
  }
  and fields = list((loc(Longident.t), expression))
  and metadata = {
    pexp_loc: Location.t,
    pexp_loc_stack: list(Location.t),
    pexp_attributes: list(attribute),
  };
};

module FieldValidator = {
  type t =
    | SyncValidator(result(sync, unit))
    | AsyncValidator({
        mode: AsyncMode.t,
        optionality: option(FieldOptionality.t),
      })
  and sync =
    | Required
    | Optional(option(unit));
};

module Scheme = {
  type t = list(entry)
  and entry =
    | Field({
        name: string,
        input_type: FieldType.t,
        output_type: FieldType.t,
        validator: FieldValidator.t,
        deps: list(Field.t),
      });
};

module InputType = {
  module T: {type t;} = {
    type t = type_declaration;
  };

  type t = T.t;
  external make: type_declaration => t = "%identity";
  external type_declaration: t => type_declaration = "%identity";
};

module OutputType = {
  module T: {type t;} = {
    type t = type_declaration;
  };

  type t = T.t;
  external make: type_declaration => t = "%identity";
  external type_declaration: t => type_declaration = "%identity";

  let default = (~loc) => [%stri type output = input];
};

module MessageType = {
  let default = (~loc) => [%stri type message = string];
};

module DebounceInterval = {
  let default = (~loc) => [%stri let debounceInterval = 700];
};

module SubmissionErrorType = {
  let default = (~loc) => [%stri type submissionError = unit];
};

module FieldOptionalityParser = {
  let parse = (typ: FieldType.t): option(FieldOptionality.t) =>
    switch (typ |> FieldType.unpack) {
    | {ptyp_desc: Ptyp_constr({txt: Lident("string")}, [])} =>
      Some(StringType)
    | {
        ptyp_desc:
          Ptyp_constr(
            {txt: Lident("option")},
            [{ptyp_desc: Ptyp_constr({txt: Lident("string")}, [])}],
          ),
      } =>
      Some(OptionStringType)
    | {ptyp_desc: Ptyp_constr({txt: Lident("option")}, _)} =>
      Some(OptionType)
    | _ => None
    };
};

module AsyncFieldParser = {
  type error =
    | InvalidPayload(Location.t)
    | InvalidAsyncMode(Location.t);

  let parse = (attributes: list(attribute)) => {
    let async_attr =
      attributes
      |> List.find_opt(attr =>
           switch (attr) {
           | {attr_name: {txt: "field.async"}} => true
           | _ => false
           }
         );
    switch (async_attr) {
    | None => Ok(None)
    | Some({attr_payload: PStr([]), attr_loc}) =>
      Ok(Some(AsyncMode.default))
    | Some({
        attr_payload:
          PStr([
            {
              pstr_desc:
                Pstr_eval(
                  {
                    pexp_desc:
                      Pexp_record(
                        [
                          (
                            {txt: Lident("mode")},
                            {
                              pexp_desc:
                                Pexp_construct(
                                  {txt: Lident(mode), loc},
                                  None,
                                ),
                            },
                          ),
                        ],
                        None,
                      ),
                  },
                  _,
                ),
            },
          ]),
        attr_loc,
      }) =>
      switch (mode) {
      | "OnChange" => Ok(Some(OnChange))
      | "OnBlur" => Ok(Some(OnBlur))
      | _ => Error(InvalidAsyncMode(loc))
      }
    | Some({attr_payload: PStr([{pstr_loc}])}) =>
      Error(InvalidPayload(pstr_loc))
    | Some({attr_loc}) => Error(InvalidPayload(attr_loc))
    };
  };
};

module FieldDepsParser = {
  type unvalidated_dep = [ | `Field(string, Location.t)];

  type error =
    | DepsParseError(Location.t)
    | DepNotFound(unvalidated_dep)
    | DepOfItself(unvalidated_dep)
    | DepDuplicate(unvalidated_dep);

  let parse = (attributes: list(attribute)) => {
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

  let validate =
      (
        fields:
          list(
            (
              Field.t,
              FieldType.t,
              option(AsyncMode.t),
              list(unvalidated_dep),
            ),
          ),
      ) =>
    fields
    |> List.fold_left(
         (res, (field: Field.t, _, _, deps)) =>
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
                          fields
                          |> List.find_opt(((field: Field.t, _, _, _)) =>
                               switch (field) {
                               | Field(field) => field == dep_name
                               }
                             ),
                          field,
                        ) {
                        | (None, _) => Error(DepNotFound(dep))
                        | (Some(_), Field(field)) =>
                          dep_name != field ? Ok() : Error(DepOfItself(dep))
                        }
                      | _ => Error(DepDuplicate(dep))
                      }
                    },
                  Ok(),
                )
           },
         Ok(),
       );
};

module InputTypeParser = {
  type result = Pervasives.result(ok, error)
  and ok = {
    fields,
    type_declaration: InputType.t,
  }
  and fields =
    list(
      (
        Field.t,
        FieldType.t,
        option(AsyncMode.t),
        list(FieldDepsParser.unvalidated_dep),
      ),
    )
  and error =
    | NotFound
    | NotRecord(Location.t)
    | InvalidAsyncField(AsyncFieldParser.error)
    | InvalidFieldDeps(FieldDepsParser.error);

  let parse = (~decl, ~rec_flag, ~loc, fields) => {
    let fields =
      List.fold_right(
        (field, res) =>
          switch (
            res,
            field.pld_type.ptyp_attributes |> AsyncFieldParser.parse,
            field.pld_type.ptyp_attributes |> FieldDepsParser.parse,
          ) {
          | (Ok(fields), Ok(async), Ok(deps)) =>
            Ok([
              (
                field |> Field.make,
                field.pld_type |> FieldType.make,
                async,
                deps,
              ),
              ...fields,
            ])
          | (Error(error), _, _) => Error(error)
          | (_, Error(error), _) => Error(InvalidAsyncField(error))
          | (_, _, Error(error)) => Error(InvalidFieldDeps(error))
          },
        fields,
        Ok([]),
      );
    switch (fields) {
    | Error(error) => Error(error)
    | Ok(fields) => Ok({fields, type_declaration: decl |> InputType.make})
    };
  };

  let in_deps_of = (fields: fields, field: Field.t) =>
    fields
    |> List.find_opt(((field', _, _, deps)) =>
         if (field |> Field.eq(field')) {
           false;
         } else {
           switch (
             deps
             |> List.find_opt(dep =>
                  switch (dep, field) {
                  | (`Field(dep, _), Field(field)) => dep == field
                  }
                )
           ) {
           | Some(_) => true
           | None => false
           };
         }
       );
};

module OutputTypeParser = {
  type result = Pervasives.result(ok, error)
  and ok =
    | NotProvided
    | AliasOfInput
    | Record({
        fields: list((Field.t, FieldType.t, Location.t)),
        loc: Location.t,
      })
  and error =
    | NotRecord(Location.t)
    | BadTypeAlias({
        alias: string,
        loc: Location.t,
      });

  let parse_as_record = (~decl, ~loc, fields) =>
    Record({
      loc,
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
    });
};

module DebounceIntervalParser = {
  let exists = (values: list(value_binding)) =>
    values
    |> List.exists(
         fun
         | {pvb_pat: {ppat_desc: Ppat_var({txt: "debounceInterval"})}} =>
           true
         | _ => false,
       );
};

module ValidatorsRecordParser = {
  type result = Pervasives.result(ValidatorsRecord.t, error)
  and error =
    | NotFound
    | NotRecord(Location.t)
    | BadTypeAnnotation(Location.t)
    | ValidatorError(
        [
          | `BadRequiredValidator(
              Field.t,
              [ | `Some(Location.t) | `None(Location.t)],
              [
                | `IncludedInDeps(Field.t)
                | `DifferentIO(FieldType.t, FieldType.t)
              ],
            )
        ],
      )
    | RecordParseError(Location.t);

  let exists = (values: list(value_binding)) =>
    values
    |> List.exists(
         fun
         | {pvb_pat: {ppat_desc: Ppat_var({txt: "validators"})}} => true
         | _ => false,
       );

  let parse = (~rec_flag, values: list(value_binding)): option(result) => {
    values
    |> List.fold_left(
         (res, value) =>
           switch (res) {
           | Some(_) => res
           | None =>
             switch (value) {
             | {
                 pvb_pat: {ppat_desc: Ppat_var({txt: "validators"})},
                 pvb_expr: {
                   pexp_desc:
                     Pexp_constraint(
                       expr,
                       {ptyp_desc: Ptyp_constr(typ, args), ptyp_loc} as annotation,
                     ),
                   pexp_loc: constraint_pexp_loc,
                   pexp_loc_stack: constraint_pexp_loc_stack,
                   pexp_attributes: constraint_pexp_attributes,
                 },
               } =>
               switch (typ, args) {
               | ({txt: Lident("validators")}, []) =>
                 switch (expr) {
                 | {
                     pexp_desc: Pexp_record(fields, None),
                     pexp_loc: record_pexp_loc,
                     pexp_loc_stack: record_pexp_loc_stack,
                     pexp_attributes: record_pexp_attributes,
                   } =>
                   Some(
                     Ok(
                       ValidatorsRecord.{
                         fields,
                         rec_flag,
                         annotation,
                         constraint_metadata: {
                           pexp_loc: constraint_pexp_loc,
                           pexp_loc_stack: constraint_pexp_loc_stack,
                           pexp_attributes: constraint_pexp_attributes,
                         },
                         record_metadata: {
                           pexp_loc: record_pexp_loc,
                           pexp_loc_stack: record_pexp_loc_stack,
                           pexp_attributes: record_pexp_attributes,
                         },
                       },
                     ),
                   )
                 | {pexp_loc} => Some(Error(NotRecord(pexp_loc)))
                 }
               | ({txt: _}, _) => Some(Error(BadTypeAnnotation(ptyp_loc)))
               }
             | {
                 pvb_pat: {ppat_desc: Ppat_var({txt: "validators"})},
                 pvb_expr: {pexp_loc} as expr,
               } =>
               switch (expr) {
               | {
                   pexp_desc: Pexp_record(fields, None),
                   pexp_loc: loc,
                   pexp_loc_stack,
                   pexp_attributes,
                 } =>
                 Some(
                   Ok({
                     fields,
                     rec_flag,
                     annotation: [%type: validators],
                     constraint_metadata: {
                       pexp_loc,
                       pexp_loc_stack,
                       pexp_attributes,
                     },
                     record_metadata: {
                       pexp_loc,
                       pexp_loc_stack,
                       pexp_attributes,
                     },
                   }),
                 )
               | {pexp_loc} => Some(Error(NotRecord(pexp_loc)))
               }
             | _ => None
             }
           },
         None,
       );
  };

  let find = (field: Field.t, validators: ValidatorsRecord.fields) =>
    validators
    |> List.find_opt(validator =>
         switch (field, validator) {
         | (Field(field), ({txt: Lident(field')}, _)) => field == field'
         | (Field(_), ({txt: _}, _)) => false
         }
       );

  let required = (field: Field.t, validators: ValidatorsRecord.fields) => {
    switch (field, validators |> find(field)) {
    | (Field(field), Some((_, {pexp_desc: Pexp_record(_)}))) => Ok()
    | (
        Field(field),
        Some((
          _,
          {
            pexp_desc:
              Pexp_construct(
                {txt: Lident("Some")},
                Some({pexp_desc: Pexp_record(_)}),
              ),
            pexp_loc,
          },
        )),
      ) =>
      Error(`Some(pexp_loc))
    | (
        Field(field),
        Some((
          _,
          {
            pexp_desc: Pexp_construct({txt: Lident("None")}, None),
            pexp_loc,
          },
        )),
      ) =>
      Error(`None(pexp_loc))
    // Don't know what it is, let compiler do the job
    | (Field(field), Some(_)) => Error(`BadValue)
    // Validator doesn't exist, delegating to compiler
    | (Field(field), None) => Error(`NotFound)
    };
  };

  let optional = (field: Field.t, validators: ValidatorsRecord.fields) => {
    switch (field, validators |> find(field)) {
    | (Field(field), Some((_, {pexp_desc: Pexp_record(_)}))) => Ok(Some())
    | (
        Field(field),
        Some((
          _,
          {pexp_desc: Pexp_construct({txt: Lident("None")}, None)},
        )),
      ) =>
      Ok(None)
    | (Field(field), Some(_)) => Error(`BadValue)
    | (Field(field), None) => Error(`NotFound)
    };
  };
};

module Metadata = {
  type t = {
    scheme: Scheme.t,
    async: bool, // meh, it should be variant: Sync(_) | Async(_)
    output_type: OutputTypeParser.ok,
    validators_record: ValidatorsRecord.t,
    message_type: option(unit),
    submission_error_type: option(unit),
    debounce_interval: option(unit),
  };

  type error =
    | InputTypeParseError(InputTypeParser.error)
    | OutputTypeParseError(OutputTypeParser.error)
    | ValidatorsRecordParseError(ValidatorsRecordParser.error)
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
    let output_parsing_result: ref(OutputTypeParser.result) =
      ref(Ok(OutputTypeParser.NotProvided));
    let validators_record_parsing_result:
      ref(option(ValidatorsRecordParser.result)) =
      ref(None);
    let message_type: ref(option(unit)) = ref(None);
    let submission_error_type: ref(option(unit)) = ref(None);
    let debounce_interval_value: ref(option(unit)) = ref(None);

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
                        fields
                        |> InputTypeParser.parse(
                             ~decl,
                             ~rec_flag,
                             ~loc=ptype_loc,
                           ),
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
                      Ok(
                        fields
                        |> OutputTypeParser.parse_as_record(
                             ~decl,
                             ~loc=ptype_loc,
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
                    } =>
                    output_parsing_result := Ok(AliasOfInput)
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
                      Error(OutputTypeParser.BadTypeAlias({alias, loc}))
                  | {ptype_name: {txt: "output"}, ptype_loc} =>
                    output_parsing_result :=
                      Error(OutputTypeParser.NotRecord(ptype_loc))

                  // Message type
                  | {
                      ptype_name: {txt: "message"},
                      ptype_loc,
                      ptype_manifest: Some(_),
                    } =>
                    message_type := Some()

                  // Submission error type
                  | {
                      ptype_name: {txt: "submissionError"},
                      ptype_loc,
                      ptype_manifest: Some(_),
                    } =>
                    submission_error_type := Some()

                  // Rest
                  | _ => (),
                );
           }
         | {pstr_desc: Pstr_value(rec_flag, values)} => {
             if (values |> DebounceIntervalParser.exists) {
               debounce_interval_value := Some();
             };
             switch (values |> ValidatorsRecordParser.parse(~rec_flag)) {
             | Some(x) => validators_record_parsing_result := Some(x)
             | None => ()
             };
           }
         | _ => (),
       );

    switch (
      input_parsing_result^,
      output_parsing_result^,
      validators_record_parsing_result^,
    ) {
    | (Some(Error(error)), _, _) => Error(InputTypeParseError(error))
    | (None, _, _) => Error(InputTypeParseError(NotFound))
    | (_, Error(error), _) => Error(OutputTypeParseError(error))
    | (_, _, None) => Error(ValidatorsRecordParseError(NotFound))
    | (_, _, Some(Error(error))) =>
      Error(ValidatorsRecordParseError(error))
    | (
        Some(Ok(input_data)),
        Ok(output_result),
        Some(Ok(validators_record)),
      ) =>
      switch (input_data.fields |> FieldDepsParser.validate) {
      | Error(error) => Error(InputTypeParseError(InvalidFieldDeps(error)))
      | Ok () =>
        let scheme: result(Scheme.t, error) =
          switch (output_result) {
          | NotProvided
          | AliasOfInput =>
            input_data.fields
            |> List.fold_left(
                 (res, (field, input_type, async, deps)) => {
                   switch (res) {
                   | Error(error) => Error(error)
                   | Ok(scheme) =>
                     let validator: result(FieldValidator.t, error) =
                       switch (async) {
                       | None =>
                         switch (
                           field
                           |> InputTypeParser.in_deps_of(input_data.fields)
                         ) {
                         | Some((in_deps_of_field, _, _, _)) =>
                           switch (
                             validators_record.fields
                             |> ValidatorsRecordParser.required(field)
                           ) {
                           | Ok () => Ok(SyncValidator(Ok(Required)))
                           | Error(`NotFound | `BadValue) =>
                             // Proceeding here since compiler
                             // would give more insightful error message
                             Ok(SyncValidator(Error()))
                           | Error(`Some(_) as reason | `None(_) as reason) =>
                             // In this case we can give more insights (hopefully)
                             // on how to fix this error
                             Error(
                               ValidatorsRecordParseError(
                                 ValidatorError(
                                   `BadRequiredValidator((
                                     field,
                                     reason,
                                     `IncludedInDeps(in_deps_of_field),
                                   )),
                                 ),
                               ),
                             )
                           }
                         | None =>
                           switch (
                             validators_record.fields
                             |> ValidatorsRecordParser.optional(field)
                           ) {
                           | Ok(res) =>
                             Ok(SyncValidator(Ok(Optional(res))))
                           | Error(`NotFound | `BadValue) =>
                             // Proceeding here since compiler
                             // would give more insightful error message
                             Ok(SyncValidator(Error()))
                           }
                         }
                       | Some(mode) =>
                         Ok(
                           AsyncValidator({
                             mode,
                             optionality:
                               input_type |> FieldOptionalityParser.parse,
                           }),
                         )
                       };
                     switch (field, validator) {
                     | (Field(field), Ok(validator)) =>
                       Ok([
                         Scheme.Field({
                           name: field,
                           input_type,
                           output_type: input_type,
                           validator,
                           deps:
                             deps
                             |> List.map(
                                  fun
                                  | `Field(dep, _) => Field.Field(dep),
                                ),
                         }),
                         ...scheme,
                       ])
                     | (_, Error(error)) => Error(error)
                     };
                   }
                 },
                 Ok([]),
               )
          | Record({fields: output_fields, loc: output_loc}) =>
            let (
              matched_fields,
              input_fields_not_in_output,
              output_fields_not_in_input,
            ) =
              List.fold_right(
                (
                  (
                    input_field,
                    input_field_type,
                    input_field_async_mode,
                    input_field_deps,
                  ),
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
                  switch (matched_fields, output_field) {
                  | (_, None) => (
                      matched_fields,
                      [input_field, ...input_fields_not_in_output],
                      output_fields_not_in_input,
                    )
                  | (
                      Error(error),
                      Some((output_field, output_field_type, _)),
                    ) => (
                      Error(error),
                      input_fields_not_in_output,
                      output_fields_not_in_input
                      |> List.filter(((output_field, _, _)) =>
                           !(input_field |> Field.eq(output_field))
                         ),
                    )

                  | (Ok(scheme), Some((output_field, output_field_type, _))) =>
                    let validator: result(FieldValidator.t, error) =
                      switch (input_field_async_mode) {
                      | None =>
                        switch (
                          input_field
                          |> InputTypeParser.in_deps_of(input_data.fields)
                        ) {
                        | Some((in_deps_of_field, _, _, _)) =>
                          switch (
                            validators_record.fields
                            |> ValidatorsRecordParser.required(input_field)
                          ) {
                          | Ok () => Ok(SyncValidator(Ok(Required)))
                          | Error(`NotFound | `BadValue) =>
                            // Proceeding here since compiler
                            // would give more insightful error message
                            Ok(SyncValidator(Error()))
                          | Error(`Some(_) as reason | `None(_) as reason) =>
                            // In this case we can give more insights (hopefully)
                            // on how to fix this error
                            Error(
                              ValidatorsRecordParseError(
                                ValidatorError(
                                  `BadRequiredValidator((
                                    input_field,
                                    reason,
                                    `IncludedInDeps(in_deps_of_field),
                                  )),
                                ),
                              ),
                            )
                          }
                        | None =>
                          if (FieldType.eq(
                                input_field_type,
                                output_field_type,
                              )) {
                            switch (
                              validators_record.fields
                              |> ValidatorsRecordParser.optional(input_field)
                            ) {
                            | Ok(res) =>
                              Ok(SyncValidator(Ok(Optional(res))))
                            | Error(`NotFound | `BadValue) =>
                              // Proceeding here since compiler
                              // would give more insightful error message
                              Ok(SyncValidator(Error()))
                            };
                          } else {
                            switch (
                              validators_record.fields
                              |> ValidatorsRecordParser.required(input_field)
                            ) {
                            | Ok () => Ok(SyncValidator(Ok(Required)))
                            | Error(`NotFound | `BadValue) =>
                              // Proceeding here since compiler
                              // would give more insightful error message
                              Ok(SyncValidator(Error()))
                            | Error(`Some(_) as reason | `None(_) as reason) =>
                              // In this case we can give more insights (hopefully)
                              // on how to fix this error
                              Error(
                                ValidatorsRecordParseError(
                                  ValidatorError(
                                    `BadRequiredValidator((
                                      input_field,
                                      reason,
                                      `DifferentIO((
                                        input_field_type,
                                        output_field_type,
                                      )),
                                    )),
                                  ),
                                ),
                              )
                            };
                          }
                        }
                      | Some(mode) =>
                        Ok(
                          AsyncValidator({
                            mode,
                            optionality:
                              output_field_type |> FieldOptionalityParser.parse,
                          }),
                        )
                      };

                    (
                      switch (input_field, validator) {
                      | (_, Error(error)) => Error(error)
                      | (Field(field), Ok(validator)) =>
                        Ok([
                          Scheme.Field({
                            name: field,
                            input_type: input_field_type,
                            output_type: output_field_type,
                            validator,
                            deps:
                              input_field_deps
                              |> List.map(
                                   fun
                                   | `Field(dep, _) => Field.Field(dep),
                                 ),
                          }),
                          ...scheme,
                        ])
                      },
                      input_fields_not_in_output,
                      output_fields_not_in_input
                      |> List.filter(((output_field, _, _)) =>
                           !(input_field |> Field.eq(output_field))
                         ),
                    );
                  };
                },
                input_data.fields,
                (Ok([]), [], output_fields),
              );
            switch (input_fields_not_in_output, output_fields_not_in_input) {
            | ([], []) => matched_fields
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

        switch (scheme) {
        | Ok(scheme) =>
          Ok({
            scheme,
            async:
              // TODO: Quick and dirty.
              //       Scheme.t should be wrapped in variant instead, probably.
              //       Let's do base implementation first,
              //       then look into how to redesign it better
              scheme
              |> List.exists((entry: Scheme.entry) =>
                   switch (entry) {
                   | Field({validator: AsyncValidator(_)}) => true
                   | Field({validator: SyncValidator(_)}) => false
                   }
                 ),
            output_type: output_result,
            validators_record,
            message_type: message_type^,
            submission_error_type: submission_error_type^,
            debounce_interval: debounce_interval_value^,
          })
        | Error(error) => Error(error)
        };
      }
    };
  };
};
