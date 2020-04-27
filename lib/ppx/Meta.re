open Ast;

open Ppxlib;

module ItemType = {
  module T: {type t;} = {
    type t = core_type;
  };

  type t = T.t;

  external make: core_type => t = "%identity";
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

module Collection = {
  type t = {
    singular: string,
    plural: string,
  };
};

module FieldDep = {
  type t =
    | DepField(string)
    | DepFieldOfCollection({
        collection: Collection.t,
        field: string,
      });

  type unvalidated =
    | UnvalidatedDepField({
        name: string,
        loc: Location.t,
      })
    | UnvalidatedDepFieldOfCollection({
        collection: string,
        field: string,
        c_loc: Location.t,
        f_loc: Location.t,
      });
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

module CollectionValidator = {
  type t = result(option(unit), unit);
};

module Scheme = {
  type t = list(entry)
  and entry =
    | Field(field)
    | Collection(collection)
  and field = {
    name: string,
    input_type: ItemType.t,
    output_type: ItemType.t,
    validator: FieldValidator.t,
    deps: list(FieldDep.t),
  }
  and collection = {
    collection: Collection.t,
    fields: list(field),
    validator: CollectionValidator.t,
    input_type: ItemType.t,
    output_type: ItemType.t,
  };

  let fields = (scheme: t) =>
    scheme
    |> List.fold_left(
         (acc, entry) =>
           switch (entry) {
           | Field(field) => [field, ...acc]
           | Collection(_) => acc
           },
         [],
       );

  let collections = (scheme: t) =>
    scheme
    |> List.fold_left(
         (acc, entry) =>
           switch (entry) {
           | Field(_) => acc
           | Collection(collection) => [collection, ...acc]
           },
         [],
       );
};

module InputFieldData = {
  type unvalidated = {
    name: string,
    typ: ItemType.t,
    async: option(AsyncMode.t),
    deps: list(FieldDep.unvalidated),
  };

  type validated = {
    name: string,
    typ: ItemType.t,
    async: option(AsyncMode.t),
    deps: list(FieldDep.t),
  };

  let unvalidated = (~async, ~deps, field: label_declaration): unvalidated => {
    name: field.pld_name.txt,
    typ: field.pld_type |> ItemType.make,
    async,
    deps,
  };

  let validated = (~deps, field: unvalidated): validated => {
    name: field.name,
    typ: field.typ,
    async: field.async,
    deps,
  };
};

module InputField = {
  type unvalidated =
    | UnvalidatedInputField(InputFieldData.unvalidated)
    | UnvalidatedInputFieldOfCollection({
        collection: Collection.t,
        field: InputFieldData.unvalidated,
      });

  type validated =
    | ValidatedInputField(InputFieldData.validated)
    | ValidatedInputFieldOfCollection({
        collection: Collection.t,
        field: InputFieldData.validated,
      });
};

module OutputFieldData = {
  type t = {
    name: string,
    typ: ItemType.t,
    loc: Location.t,
  };
};

module OutputField = {
  type t =
    | OutputField(OutputFieldData.t)
    | OutputFieldOfCollection({
        collection: Collection.t,
        field: OutputFieldData.t,
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
  let parse = (typ: ItemType.t): option(FieldOptionality.t) =>
    switch (typ |> ItemType.unpack) {
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

  let attr = field =>
    field.pld_type.ptyp_attributes
    |> List.find_opt(attr =>
         switch (attr) {
         | {attr_name: {txt: "field.async"}} => true
         | _ => false
         }
       );

  let parse = attribute => {
    switch (attribute) {
    | {attr_payload: PStr([]), attr_loc} => Ok(AsyncMode.default)
    | {
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
      } =>
      switch (mode) {
      | "OnChange" => Ok(OnChange)
      | "OnBlur" => Ok(OnBlur)
      | _ => Error(InvalidAsyncMode(loc))
      }
    | {attr_payload: PStr([{pstr_loc}])} =>
      Error(InvalidPayload(pstr_loc))
    | {attr_loc} => Error(InvalidPayload(attr_loc))
    };
  };

  let get = field =>
    switch (field |> attr) {
    | None => Ok(None)
    | Some(attr) =>
      switch (attr |> parse) {
      | Ok(mode) => Ok(Some(mode))
      | Error(error) => Error(error)
      }
    };
};

module FieldDepsParser = {
  type error =
    | DepsParseError(Location.t)
    | DepNotFound(FieldDep.unvalidated)
    | DepOfItself([ | `Field(string, Location.t)])
    | DepDuplicate(FieldDep.unvalidated);

  let attr = field =>
    field.pld_type.ptyp_attributes
    |> List.find_opt(attr =>
         switch (attr) {
         | {attr_name: {txt: "field.deps"}} => true
         | _ => false
         }
       );

  let parse =
      (attribute: attribute): result(list(FieldDep.unvalidated), error) => {
    switch (attribute) {
    | {attr_payload: PStr([{pstr_desc: Pstr_eval(exp, _)}]), attr_loc} =>
      switch (exp) {
      | {pexp_desc: Pexp_ident({txt: Lident(dep), loc})} =>
        Ok([UnvalidatedDepField({name: dep, loc})])
      | {
          pexp_desc:
            Pexp_field(
              {
                pexp_desc: Pexp_ident({txt: Lident(collection), loc: c_loc}),
              },
              {txt: Lident(field), loc: f_loc},
            ),
        } =>
        Ok([
          UnvalidatedDepFieldOfCollection({collection, field, c_loc, f_loc}),
        ])
      | {pexp_desc: Pexp_tuple(exps)} =>
        exps
        |> List.fold_left(
             (res: result(list(FieldDep.unvalidated), error), exp) =>
               switch (res, exp) {
               | (Error(error), _) => Error(error)
               | (
                   Ok(deps),
                   {pexp_desc: Pexp_ident({txt: Lident(dep), loc})},
                 ) =>
                 Ok([UnvalidatedDepField({name: dep, loc}), ...deps])
               | (
                   Ok(deps),
                   {
                     pexp_desc:
                       Pexp_field(
                         {
                           pexp_desc:
                             Pexp_ident({
                               txt: Lident(collection),
                               loc: c_loc,
                             }),
                         },
                         {txt: Lident(field), loc: f_loc},
                       ),
                   },
                 ) =>
                 Ok([
                   UnvalidatedDepFieldOfCollection({
                     collection,
                     field,
                     c_loc,
                     f_loc,
                   }),
                   ...deps,
                 ])
               | (Ok(_), {pexp_loc}) => Error(DepsParseError(pexp_loc))
               },
             Ok([]),
           )
      | {pexp_loc} => Error(DepsParseError(pexp_loc))
      }
    | {attr_loc} => Error(DepsParseError(attr_loc))
    };
  };

  let get = field =>
    switch (field |> attr) {
    | None => Ok([])
    | Some(attr) => attr |> parse
    };
};

module FieldCollectionParser = {
  type result = Pervasives.result(ok, error)
  and ok = {
    collection: Collection.t,
    fields: list(InputFieldData.unvalidated),
    input_type: ItemType.t,
  }
  and error =
    | NotArray(Location.t)
    | InvalidTypeRef(Location.t)
    | RecordNotFound(Location.t)
    | NotRecord(Location.t)
    | InvalidAsyncField(AsyncFieldParser.error)
    | InvalidFieldDeps(FieldDepsParser.error);

  let attr = (field: label_declaration) =>
    field.pld_type.ptyp_attributes
    |> List.find_opt(attr =>
         switch (attr) {
         | {attr_name: {txt: "field.collection"}} => true
         | _ => false
         }
       );

  let parse = (~structure: structure, field: label_declaration): result => {
    switch (field.pld_type.ptyp_desc) {
    | Ptyp_constr({txt: Lident("array"), loc: arr_loc}, payload) =>
      switch (payload) {
      | [] => Error(InvalidTypeRef(arr_loc))
      | [
          {ptyp_desc: Ptyp_constr({txt: Lident(typ_name)}, []), ptyp_loc} as input_type,
          ..._,
        ] =>
        let record_type = ref(None);
        structure
        |> List.iter((item: structure_item) =>
             switch (item) {
             | {pstr_desc: Pstr_type(rec_flag, decls)} =>
               decls
               |> List.iter((decl: type_declaration) =>
                    switch (decl) {
                    | {ptype_name: {txt: name}} when name == typ_name =>
                      switch (decl.ptype_kind) {
                      | Ptype_record(fields) =>
                        record_type := Some(Ok(fields))
                      | _ =>
                        record_type :=
                          Some(Error(NotRecord(decl.ptype_loc)))
                      }
                    | _ => ()
                    }
                  )
             | _ => ()
             }
           );
        switch (record_type^) {
        | None => Error(RecordNotFound(ptyp_loc))
        | Some(Error(error)) => Error(error)
        | Some(Ok(fields)) =>
          let fields =
            fields
            |> List.fold_left(
                 (res, field: label_declaration) =>
                   switch (res) {
                   | Error(error) => Error(error)
                   | Ok(fields) =>
                     switch (
                       field |> AsyncFieldParser.get,
                       field |> FieldDepsParser.get,
                     ) {
                     | (Ok(async), Ok(deps)) =>
                       Ok([
                         field |> InputFieldData.unvalidated(~async, ~deps),
                         ...fields,
                       ])
                     | (Error(error), _) => Error(InvalidAsyncField(error))
                     | (_, Error(error)) => Error(InvalidFieldDeps(error))
                     }
                   },
                 Ok([]),
               );
          switch (fields) {
          | Ok(fields) =>
            Ok({
              collection: {
                plural: field.pld_name.txt,
                singular: typ_name,
              },
              fields,
              input_type: input_type |> ItemType.make,
            })
          | Error(error) => Error(error)
          };
        };
      | [{ptyp_loc}, ..._] => Error(InvalidTypeRef(ptyp_loc))
      }
    | _ => Error(NotArray(field.pld_loc))
    };
  };
};

module FieldAttributesParser = {
  type result = Pervasives.result(option(ok), error)
  and ok =
    | Collection(FieldCollectionParser.ok)
    | AsyncDeps({
        async: option(AsyncMode.t),
        deps: list(FieldDep.unvalidated),
      })
  and error =
    | Conflict(
        [
          | `AsyncWithCollection(Location.t)
          | `DepsWithCollection(Location.t)
        ],
      )
    | InvalidCollectionField(FieldCollectionParser.error)
    | InvalidAsyncField(AsyncFieldParser.error)
    | InvalidFieldDeps(FieldDepsParser.error);

  let parse = (~structure: structure, field: label_declaration) =>
    switch (
      field |> FieldCollectionParser.attr,
      field |> AsyncFieldParser.attr,
      field |> FieldDepsParser.attr,
    ) {
    | (Some(_), None, None) =>
      switch (field |> FieldCollectionParser.parse(~structure)) {
      | Ok(collection) => Ok(Some(Collection(collection)))
      | Error(error) => Error(InvalidCollectionField(error))
      }
    | (None, Some(async_attr), Some(deps_attr)) =>
      switch (
        async_attr |> AsyncFieldParser.parse,
        deps_attr |> FieldDepsParser.parse,
      ) {
      | (Ok(async), Ok(deps)) =>
        Ok(Some(AsyncDeps({async: Some(async), deps})))
      | (Error(error), _) => Error(InvalidAsyncField(error))
      | (_, Error(error)) => Error(InvalidFieldDeps(error))
      }
    | (None, Some(async_attr), None) =>
      switch (async_attr |> AsyncFieldParser.parse) {
      | Ok(async) => Ok(Some(AsyncDeps({async: Some(async), deps: []})))
      | Error(error) => Error(InvalidAsyncField(error))
      }
    | (None, None, Some(deps_attr)) =>
      switch (deps_attr |> FieldDepsParser.parse) {
      | Ok(deps) => Ok(Some(AsyncDeps({async: None, deps})))
      | Error(error) => Error(InvalidFieldDeps(error))
      }
    | (None, None, None) => Ok(None)
    | (Some(_), Some({attr_loc}), _) =>
      Error(Conflict(`AsyncWithCollection(attr_loc)))
    | (Some(_), _, Some({attr_loc})) =>
      Error(Conflict(`DepsWithCollection(attr_loc)))
    };
};

module InputTypeParser = {
  type result = Pervasives.result(ok, error)
  and ok = {
    entries: list(unvalidated_entry),
    type_declaration: InputType.t,
  }
  and unvalidated_entry =
    | UnvalidatedInputField(InputFieldData.unvalidated)
    | UnvalidatedInputCollection({
        collection: Collection.t,
        fields: list(InputFieldData.unvalidated),
        input_type: ItemType.t,
      })
  and validated_entry =
    | ValidatedInputField(InputFieldData.validated)
    | ValidatedInputCollection({
        collection: Collection.t,
        fields: list(InputFieldData.validated),
        input_type: ItemType.t,
      })
  and error =
    | NotFound
    | NotRecord(Location.t)
    | InvalidAttributes(FieldAttributesParser.error);

  let parse = (~decl, ~structure, ~loc, fields) => {
    let entries =
      fields
      |> List.rev
      |> List.fold_left(
           (res, field) =>
             switch (res, field |> FieldAttributesParser.parse(~structure)) {
             | (
                 Ok(entries),
                 Ok(Some(Collection({collection, fields, input_type}))),
               ) =>
               Ok([
                 UnvalidatedInputCollection({collection, fields, input_type}),
                 ...entries,
               ])
             | (Ok(entries), Ok(Some(AsyncDeps({async, deps})))) =>
               Ok([
                 UnvalidatedInputField(
                   field |> InputFieldData.unvalidated(~async, ~deps),
                 ),
                 ...entries,
               ])
             | (Ok(entries), Ok(None)) =>
               Ok([
                 UnvalidatedInputField(
                   field |> InputFieldData.unvalidated(~async=None, ~deps=[]),
                 ),
                 ...entries,
               ])
             | (Error(error), _) => Error(error)
             | (_, Error(error)) => Error(InvalidAttributes(error))
             },
           Ok([]),
         );
    switch (entries) {
    | Error(error) => Error(error)
    | Ok(entries) => Ok({entries, type_declaration: decl |> InputType.make})
    };
  };

  let validate =
      (unvalidated_entries: list(unvalidated_entry))
      : Pervasives.result(list(validated_entry), FieldDepsParser.error) => {
    let dup = (deps: list(FieldDep.unvalidated), dep: FieldDep.unvalidated) =>
      switch (
        deps
        |> List.find_all((dep': FieldDep.unvalidated) =>
             switch (dep, dep') {
             | (
                 UnvalidatedDepField({name: dep}),
                 UnvalidatedDepField({name: dep'}),
               ) =>
               dep == dep
             | (
                 UnvalidatedDepFieldOfCollection({collection, field}),
                 UnvalidatedDepFieldOfCollection({
                   collection: collection',
                   field: field',
                 }),
               ) =>
               collection == collection' && field == field'
             | (UnvalidatedDepField(_), UnvalidatedDepFieldOfCollection(_))
             | (UnvalidatedDepFieldOfCollection(_), UnvalidatedDepField(_)) =>
               false
             }
           )
        |> List.length
      ) {
      | 0
      | 1 => None
      | _ => Some()
      };

    unvalidated_entries
    |> List.fold_left(
         (
           res:
             Pervasives.result(list(validated_entry), FieldDepsParser.error),
           unvalidated_entry: unvalidated_entry,
         ) =>
           switch (res, unvalidated_entry) {
           | (Error(error), _) => Error(error)
           | (Ok(validated_entries), UnvalidatedInputField(field)) =>
             let deps =
               field.deps
               |> List.fold_left(
                    (
                      res:
                        Pervasives.result(
                          list(FieldDep.t),
                          FieldDepsParser.error,
                        ),
                      dep,
                    ) =>
                      switch (res) {
                      | Error(error) => Error(error)
                      | Ok(validated_deps) =>
                        switch (dep |> dup(field.deps)) {
                        | Some () => Error(FieldDepsParser.DepDuplicate(dep))
                        | None =>
                          switch (
                            unvalidated_entries
                            |> List.fold_left(
                                 (
                                   res:
                                     option(
                                       Pervasives.result(
                                         FieldDep.t,
                                         FieldDepsParser.error,
                                       ),
                                     ),
                                   entry,
                                 ) =>
                                   switch (res, dep, entry) {
                                   | (Some(_) as res, _, _) => res
                                   | (
                                       None,
                                       UnvalidatedDepField(dep'),
                                       UnvalidatedInputField(field'),
                                     ) =>
                                     if (field.name == field'.name
                                         && field'.name == dep'.name) {
                                       Some(
                                         Error(
                                           FieldDepsParser.DepOfItself(
                                             `Field((dep'.name, dep'.loc)),
                                           ),
                                         ),
                                       );
                                     } else if (field'.name == dep'.name) {
                                       Some(Ok(DepField(dep'.name)));
                                     } else {
                                       None;
                                     }
                                   | (
                                       None,
                                       UnvalidatedDepFieldOfCollection(dep'),
                                       UnvalidatedInputCollection(entry'),
                                     ) =>
                                     if (dep'.collection
                                         != entry'.collection.singular) {
                                       None;
                                     } else {
                                       switch (
                                         entry'.fields
                                         |> List.find_opt(
                                              (
                                                field: InputFieldData.unvalidated,
                                              ) =>
                                              dep'.field == field.name
                                            )
                                       ) {
                                       | None =>
                                         Some(
                                           Error(
                                             FieldDepsParser.DepNotFound(dep),
                                           ),
                                         )
                                       | Some(field) =>
                                         Some(
                                           Ok(
                                             DepFieldOfCollection({
                                               collection: entry'.collection,
                                               field: field.name,
                                             }),
                                           ),
                                         )
                                       };
                                     }
                                   | (
                                       None,
                                       UnvalidatedDepField(_),
                                       UnvalidatedInputCollection(_),
                                     )
                                   | (
                                       None,
                                       UnvalidatedDepFieldOfCollection(_),
                                       UnvalidatedInputField(_),
                                     ) =>
                                     None
                                   },
                                 None,
                               )
                          ) {
                          | None => Error(FieldDepsParser.DepNotFound(dep))
                          | Some(Error(error)) => Error(error)
                          | Some(Ok(dep_entry)) =>
                            Ok([dep_entry, ...validated_deps])
                          }
                        }
                      },
                    Ok([]),
                  );
             switch (deps) {
             | Error(error) => Error(error)
             | Ok(deps) =>
               Ok([
                 ValidatedInputField(
                   field |> InputFieldData.validated(~deps),
                 ),
                 ...validated_entries,
               ])
             };
           | (
               Ok(validated_entries),
               UnvalidatedInputCollection({
                 collection,
                 fields: unvalidated_fields,
                 input_type,
               }),
             ) =>
             let validated_fields =
               unvalidated_fields
               |> List.fold_left(
                    (
                      res:
                        Pervasives.result(
                          list(InputFieldData.validated),
                          FieldDepsParser.error,
                        ),
                      field: InputFieldData.unvalidated,
                    ) =>
                      switch (res) {
                      | Error(error) => Error(error)
                      | Ok(validated_fields) =>
                        let deps =
                          field.deps
                          |> List.fold_left(
                               (
                                 res:
                                   Pervasives.result(
                                     list(FieldDep.t),
                                     FieldDepsParser.error,
                                   ),
                                 dep,
                               ) =>
                                 switch (res) {
                                 | Error(error) => Error(error)
                                 | Ok(validated_deps) =>
                                   switch (dep |> dup(field.deps)) {
                                   | Some () =>
                                     Error(FieldDepsParser.DepDuplicate(dep))
                                   | None =>
                                     switch (
                                       unvalidated_entries
                                       |> List.fold_left(
                                            (
                                              res:
                                                option(
                                                  Pervasives.result(
                                                    FieldDep.t,
                                                    FieldDepsParser.error,
                                                  ),
                                                ),
                                              entry,
                                            ) =>
                                              switch (res, dep, entry) {
                                              | (Some(_) as res, _, _) => res
                                              | (
                                                  None,
                                                  UnvalidatedDepField(dep'),
                                                  UnvalidatedInputField(
                                                    field',
                                                  ),
                                                ) =>
                                                if (field'.name == dep'.name) {
                                                  Some(
                                                    Ok(DepField(dep'.name)),
                                                  );
                                                } else {
                                                  None;
                                                }
                                              | (
                                                  None,
                                                  UnvalidatedDepFieldOfCollection(
                                                    dep',
                                                  ),
                                                  UnvalidatedInputCollection(
                                                    entry',
                                                  ),
                                                ) =>
                                                if (dep'.collection
                                                    != entry'.collection.
                                                         singular) {
                                                  None;
                                                } else {
                                                  switch (
                                                    entry'.fields
                                                    |> List.fold_left(
                                                         (
                                                           res:
                                                             option(
                                                               Pervasives.result(
                                                                 FieldDep.t,
                                                                 FieldDepsParser.error,
                                                               ),
                                                             ),
                                                           field: InputFieldData.unvalidated,
                                                         ) =>
                                                           switch (res) {
                                                           | Some(_) => res
                                                           | None =>
                                                             if (dep'.field
                                                                 == field.name) {
                                                               Some(
                                                                 Ok(
                                                                   DepFieldOfCollection({
                                                                    collection:
                                                                    entry'.
                                                                    collection,
                                                                    field:
                                                                    field.name,
                                                                   }),
                                                                 ),
                                                               );
                                                             } else {
                                                               None;
                                                             }
                                                           },
                                                         None,
                                                       )
                                                  ) {
                                                  | None =>
                                                    Some(
                                                      Error(
                                                        FieldDepsParser.DepNotFound(
                                                          dep,
                                                        ),
                                                      ),
                                                    )
                                                  | Some(Error(error)) =>
                                                    Some(Error(error))
                                                  | Some(Ok(dep)) =>
                                                    Some(Ok(dep))
                                                  };
                                                }
                                              | (
                                                  None,
                                                  UnvalidatedDepField(_),
                                                  UnvalidatedInputCollection(
                                                    _
                                                  ),
                                                )
                                              | (
                                                  None,
                                                  UnvalidatedDepFieldOfCollection(
                                                    _
                                                  ),
                                                  UnvalidatedInputField(_),
                                                ) =>
                                                None
                                              },
                                            None,
                                          )
                                     ) {
                                     | None =>
                                       Error(
                                         FieldDepsParser.DepNotFound(dep),
                                       )
                                     | Some(Error(error)) => Error(error)
                                     | Some(Ok(dep_entry)) =>
                                       Ok([dep_entry, ...validated_deps])
                                     }
                                   }
                                 },
                               Ok([]),
                             );
                        switch (deps) {
                        | Error(error) => Error(error)
                        | Ok(deps) =>
                          Ok([
                            field |> InputFieldData.validated(~deps),
                            ...validated_fields,
                          ])
                        };
                      },
                    Ok([]),
                  );
             switch (validated_fields) {
             | Error(error) => Error(error)
             | Ok(validated_fields) =>
               Ok([
                 ValidatedInputCollection({
                   collection,
                   fields: validated_fields,
                   input_type,
                 }),
                 ...validated_entries,
               ])
             };
           },
         Ok([]),
       );
  };

  let in_deps_of =
      (entries: list(validated_entry), field: InputField.validated)
      : option(InputField.validated) => {
    entries
    |> List.fold_left(
         (res, entry: validated_entry) =>
           switch (res, field, entry) {
           | (Some(_), _, _) => res
           | (
               None,
               ValidatedInputField(subject_field),
               ValidatedInputField(entry_field),
             ) =>
             entry_field.deps
             |> List.fold_left(
                  (res: option(InputField.validated), dep: FieldDep.t) =>
                    switch (res, dep) {
                    | (Some(_), _) => res
                    | (None, DepField(dep)) =>
                      if (dep == subject_field.name) {
                        Some(ValidatedInputField(entry_field));
                      } else {
                        None;
                      }
                    | (None, DepFieldOfCollection(_)) => None
                    },
                  None,
                )
           | (
               None,
               ValidatedInputField(subject_field),
               ValidatedInputCollection({
                 collection: entry_collection,
                 fields: entry_fields,
               }),
             ) =>
             entry_fields
             |> List.fold_left(
                  (
                    res: option(InputField.validated),
                    entry_field: InputFieldData.validated,
                  ) =>
                    entry_field.deps
                    |> List.fold_left(
                         (res: option(InputField.validated), dep: FieldDep.t) =>
                           switch (res, dep) {
                           | (Some(_), _) => res
                           | (None, DepField(dep)) =>
                             if (dep == subject_field.name) {
                               Some(
                                 ValidatedInputFieldOfCollection({
                                   collection: entry_collection,
                                   field: entry_field,
                                 }),
                               );
                             } else {
                               None;
                             }
                           | (None, DepFieldOfCollection(_)) => None
                           },
                         None,
                       ),
                  None,
                )
           | (
               None,
               ValidatedInputFieldOfCollection({
                 collection: subject_collection,
                 field: subject_field,
               }),
               ValidatedInputField(entry_field),
             ) =>
             entry_field.deps
             |> List.fold_left(
                  (res: option(InputField.validated), dep: FieldDep.t) =>
                    switch (res, dep) {
                    | (Some(_), _) => res
                    | (None, DepField(dep)) => None
                    | (
                        None,
                        DepFieldOfCollection({
                          collection: dep_collection,
                          field: dep_field,
                        }),
                      ) =>
                      if (dep_collection.singular
                          == subject_collection.singular
                          && dep_field == subject_field.name) {
                        Some(ValidatedInputField(entry_field));
                      } else {
                        None;
                      }
                    },
                  None,
                )
           | (
               None,
               ValidatedInputFieldOfCollection({
                 collection: subject_collection,
                 field: subject_field,
               }),
               ValidatedInputCollection({
                 collection: entry_collection,
                 fields: entry_fields,
               }),
             ) =>
             entry_fields
             |> List.fold_left(
                  (
                    res: option(InputField.validated),
                    entry_field: InputFieldData.validated,
                  ) =>
                    entry_field.deps
                    |> List.fold_left(
                         (res: option(InputField.validated), dep: FieldDep.t) =>
                           switch (res, dep) {
                           | (Some(_), _) => res
                           | (None, DepField(dep)) => None
                           | (
                               None,
                               DepFieldOfCollection({
                                 collection: dep_collection,
                                 field: dep_field,
                               }),
                             ) =>
                             if (subject_collection.singular
                                 == dep_collection.singular
                                 && subject_field.name == dep_field) {
                               Some(
                                 ValidatedInputFieldOfCollection({
                                   collection: entry_collection,
                                   field: entry_field,
                                 }),
                               );
                             } else {
                               None;
                             }
                           },
                         None,
                       ),
                  None,
                )
           },
         None,
       );
  };
};

module OutputTypeParser = {
  type result = Pervasives.result(ok, error)
  and ok =
    | NotProvided
    | AliasOfInput
    | Record({
        entries: list(entry),
        loc: Location.t,
      })
  and entry =
    | OutputField(OutputFieldData.t)
    | OutputCollection({
        collection: Collection.t,
        fields: list(OutputFieldData.t),
        output_type: ItemType.t,
      })
  and error =
    | InputNotAvailable(Location.t)
    | NotRecord(Location.t)
    | BadTypeAlias({
        alias: string,
        loc: Location.t,
      })
    | OutputCollectionNotFound({
        input_collection: Collection.t,
        loc: Location.t,
      })
    | InvalidCollection(collection_error)
  and collection_error =
    | InvalidCollectionTypeRef(Location.t)
    | CollectionTypeNotRecord(Location.t)
    | CollectionTypeNotFound(Location.t)
    | CollectionOutputNotArray(Location.t);

  let flatten = (entries: list(entry)): list(OutputField.t) =>
    entries
    |> List.rev
    |> List.fold_left(
         (acc, entry) =>
           switch (entry) {
           | OutputField(field) => [OutputField.OutputField(field), ...acc]
           | OutputCollection({collection, fields}) =>
             fields
             |> List.rev
             |> List.fold_left(
                  (acc, field) =>
                    [
                      OutputField.OutputFieldOfCollection({collection, field}),
                      ...acc,
                    ],
                  acc,
                )
           },
         [],
       );

  let parse =
      (
        ~structure,
        ~input_collections: list(Collection.t),
        ~loc,
        fields: list(label_declaration),
      ) =>
    switch (input_collections) {
    | [] =>
      Ok(
        Record({
          loc,
          entries:
            fields
            |> List.rev
            |> List.fold_left(
                 (acc, field) =>
                   [
                     OutputField({
                       name: field.pld_name.txt,
                       typ: field.pld_type |> ItemType.make,
                       loc: field.pld_loc,
                     }),
                     ...acc,
                   ],
                 [],
               ),
        }),
      )
    | _ =>
      let entries =
        fields
        |> List.rev
        |> List.fold_left(
             (acc, field) =>
               switch (acc) {
               | Error(error) => Error(error)
               | Ok(entries) =>
                 let field_name = field.pld_name.txt;
                 switch (
                   input_collections
                   |> List.find_opt((collection: Collection.t) =>
                        collection.plural == field_name
                      )
                 ) {
                 | None =>
                   Ok([
                     OutputField({
                       name: field_name,
                       typ: field.pld_type |> ItemType.make,
                       loc: field.pld_loc,
                     }),
                     ...entries,
                   ])
                 | Some(input_collection) =>
                   switch (field.pld_type.ptyp_desc) {
                   | Ptyp_constr(
                       {txt: Lident("array"), loc: arr_loc},
                       payload,
                     ) =>
                     switch (payload) {
                     | [] => Error(InvalidCollectionTypeRef(arr_loc))
                     | [
                         {
                           ptyp_desc:
                             Ptyp_constr({txt: Lident(type_name)}, []),
                           ptyp_loc,
                         } as output_type,
                         ..._,
                       ] =>
                       let record_type = ref(None);
                       structure
                       |> List.iter((item: structure_item) =>
                            switch (item) {
                            | {pstr_desc: Pstr_type(rec_flag, decls)} =>
                              decls
                              |> List.iter((decl: type_declaration) =>
                                   switch (decl) {
                                   | {ptype_name: {txt: name}}
                                       when name == type_name =>
                                     switch (decl.ptype_kind) {
                                     | Ptype_record(fields) =>
                                       record_type := Some(Ok(fields))
                                     | _ =>
                                       record_type :=
                                         Some(
                                           Error(
                                             CollectionTypeNotRecord(
                                               decl.ptype_loc,
                                             ),
                                           ),
                                         )
                                     }
                                   | _ => ()
                                   }
                                 )
                            | _ => ()
                            }
                          );
                       switch (record_type^) {
                       | None => Error(CollectionTypeNotFound(ptyp_loc))
                       | Some(Error(error)) => Error(error)
                       | Some(Ok(fields)) =>
                         Ok([
                           OutputCollection({
                             collection: {
                               plural: field_name,
                               singular: type_name,
                             },
                             fields:
                               fields
                               |> List.rev
                               |> List.rev_map((field: label_declaration) =>
                                    OutputFieldData.{
                                      name: field.pld_name.txt,
                                      typ: field.pld_type |> ItemType.make,
                                      loc: field.pld_loc,
                                    }
                                  ),
                             output_type: output_type |> ItemType.make,
                           }),
                           ...entries,
                         ])
                       };
                     | [{ptyp_loc}, ..._] =>
                       Error(InvalidCollectionTypeRef(ptyp_loc))
                     }
                   | _ => Error(CollectionOutputNotArray(field.pld_loc))
                   }
                 };
               },
             Ok([]),
           );
      switch (entries) {
      | Ok(entries) => Ok(Record({loc, entries}))
      | Error(error) => Error(InvalidCollection(error))
      };
    };
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
              InputField.validated,
              [ | `Some(Location.t) | `None(Location.t)],
              [
                | `IncludedInDeps(InputField.validated)
                | `DifferentIO(ItemType.t, ItemType.t)
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

  let find_field =
      (field: InputField.validated, validators: ValidatorsRecord.fields) =>
    validators
    |> List.fold_left(
         (res, validator) =>
           switch (res, field, validator) {
           | (Some(_), _, _) => res
           | (None, ValidatedInputField(field), ({txt: Lident(key)}, _)) =>
             field.name == key ? Some(validator) : None
           | (
               None,
               ValidatedInputFieldOfCollection({collection, field}),
               (
                 {txt: Lident(key)},
                 {pexp_desc: Pexp_record(fields, None)},
               ),
             ) =>
             if (collection.plural == key) {
               fields
               |> List.fold_left(
                    (res, entry) =>
                      switch (res, entry) {
                      | (Some(_), _) => res
                      | (
                          None,
                          (
                            {txt: Lident("fields")},
                            {pexp_desc: Pexp_record(fields, None)},
                          ),
                        ) =>
                        fields
                        |> List.find_opt(entry =>
                             switch (entry) {
                             | ({txt: Lident(key)}, _) => key == field.name
                             | _ => false
                             }
                           )
                      | (None, _) => None
                      },
                    None,
                  );
             } else {
               None;
             }
           | (
               None,
               ValidatedInputFieldOfCollection({collection, field}),
               ({txt: _}, _),
             ) =>
             None
           | (None, ValidatedInputField(_), ({txt: _}, _)) => None
           },
         None,
       );

  let find_collection =
      (collection: Collection.t, validators: ValidatorsRecord.fields) =>
    validators
    |> List.fold_left(
         (res, validator) =>
           switch (res, validator) {
           | (Some(_), _) => res
           | (
               None,
               (
                 {txt: Lident(key)},
                 {pexp_desc: Pexp_record(fields, None)},
               ),
             )
               when collection.plural == key =>
             fields
             |> List.fold_left(
                  (res, entry) =>
                    switch (res, entry) {
                    | (Some(_), _) => res
                    | (None, ({txt: Lident("collection")}, exp)) =>
                      Some(exp)
                    | (None, _) => None
                    },
                  None,
                )
           | (None, ({txt: _}, _)) => None
           },
         None,
       );

  let required =
      (field: InputField.validated, validators: ValidatorsRecord.fields) => {
    switch (validators |> find_field(field)) {
    | Some((_, {pexp_desc: Pexp_record(_)})) => Ok()
    | Some((
        _,
        {
          pexp_desc:
            Pexp_construct(
              {txt: Lident("Some")},
              Some({pexp_desc: Pexp_record(_)}),
            ),
          pexp_loc,
        },
      )) =>
      Error(`Some(pexp_loc))
    | Some((
        _,
        {pexp_desc: Pexp_construct({txt: Lident("None")}, None), pexp_loc},
      )) =>
      Error(`None(pexp_loc))
    | Some(_) => Error(`BadValue)
    | None => Error(`NotFound)
    };
  };

  let optional =
      (field: InputField.validated, validators: ValidatorsRecord.fields) => {
    switch (validators |> find_field(field)) {
    | Some((_, {pexp_desc: Pexp_record(_)})) => Ok(Some())
    | Some((_, {pexp_desc: Pexp_construct({txt: Lident("None")}, None)})) =>
      Ok(None)
    | Some(_) => Error(`BadValue)
    | None => Error(`NotFound)
    };
  };

  let collection =
      (collection: Collection.t, validators: ValidatorsRecord.fields) =>
    switch (validators |> find_collection(collection)) {
    | Some({pexp_desc: Pexp_fun(_)}) => Ok(Some())
    | Some({pexp_desc: Pexp_construct({txt: Lident("None")}, None)}) =>
      Ok(None)
    | Some(_) => Error(`BadValue)
    | None => Error(`NotFound)
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
        fields: list(InputField.validated),
        loc: Location.t,
      })
    | OutputFieldsNotInInput({fields: list(OutputField.t)})
    | Both({
        input_fields_not_in_output: list(InputField.validated),
        output_fields_not_in_input: list(OutputField.t),
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
                             ~structure,
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
                    } =>
                    switch (input_parsing_result^) {
                    | None =>
                      output_parsing_result :=
                        Error(InputNotAvailable(ptype_loc))
                    | Some(Ok({entries})) =>
                      output_parsing_result :=
                        fields
                        |> OutputTypeParser.parse(
                             ~structure,
                             ~loc=ptype_loc,
                             ~input_collections=
                               entries
                               |> List.fold_left(
                                    (
                                      acc,
                                      entry: InputTypeParser.unvalidated_entry,
                                    ) =>
                                      switch (entry) {
                                      | UnvalidatedInputField(_) => acc
                                      | UnvalidatedInputCollection({
                                          collection,
                                        }) => [
                                          collection,
                                          ...acc,
                                        ]
                                      },
                                    [],
                                  ),
                           )
                    | Some(Error(_)) => ()
                    }
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
                  | {ptype_name: {txt: "message"}, ptype_loc} =>
                    message_type := Some()

                  // Submission error type
                  | {ptype_name: {txt: "submissionError"}, ptype_loc} =>
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
      switch (input_data.entries |> InputTypeParser.validate) {
      | Error(error) =>
        Error(
          InputTypeParseError(InvalidAttributes(InvalidFieldDeps(error))),
        )
      | Ok(validated_input_entries) =>
        let scheme: result(Scheme.t, error) =
          switch (output_result) {
          | NotProvided
          | AliasOfInput =>
            let validator =
                (
                  ~field: InputField.validated,
                  ~entries: list(InputTypeParser.validated_entry),
                  ~validators_record: ValidatorsRecord.t,
                  ~async_mode: option(AsyncMode.t),
                  ~output_type: ItemType.t,
                )
                : result(FieldValidator.t, error) =>
              switch (async_mode) {
              | None =>
                switch (field |> InputTypeParser.in_deps_of(entries)) {
                | Some(in_deps_of_entry) =>
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
                            `IncludedInDeps(in_deps_of_entry),
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
                  | Ok(res) => Ok(SyncValidator(Ok(Optional(res))))
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
                    optionality: output_type |> FieldOptionalityParser.parse,
                  }),
                )
              };

            validated_input_entries
            |> List.fold_left(
                 (res, entry: InputTypeParser.validated_entry) => {
                   switch (res, entry) {
                   | (Error(error), _) => Error(error)
                   | (Ok(scheme), ValidatedInputField(field)) =>
                     let validator =
                       validator(
                         ~field=ValidatedInputField(field),
                         ~entries=validated_input_entries,
                         ~validators_record,
                         ~async_mode=field.async,
                         ~output_type=field.typ,
                       );
                     switch (validator) {
                     | Ok(validator) =>
                       Ok([
                         Scheme.Field({
                           name: field.name,
                           input_type: field.typ,
                           output_type: field.typ,
                           validator,
                           deps: field.deps,
                         }),
                         ...scheme,
                       ])
                     | Error(error) => Error(error)
                     };

                   | (
                       Ok(scheme),
                       ValidatedInputCollection({
                         collection,
                         fields,
                         input_type,
                       }),
                     ) =>
                     let fields =
                       fields
                       |> List.fold_left(
                            (res, field) =>
                              switch (res) {
                              | Error(error) => Error(error)
                              | Ok(fields) =>
                                let validator =
                                  validator(
                                    ~field=
                                      ValidatedInputFieldOfCollection({
                                        collection,
                                        field,
                                      }),
                                    ~entries=validated_input_entries,
                                    ~validators_record,
                                    ~async_mode=field.async,
                                    ~output_type=field.typ,
                                  );
                                switch (validator) {
                                | Ok(validator) =>
                                  Ok([
                                    Scheme.{
                                      name: field.name,
                                      input_type: field.typ,
                                      output_type: field.typ,
                                      validator,
                                      deps: field.deps,
                                    },
                                    ...fields,
                                  ])
                                | Error(error) => Error(error)
                                };
                              },
                            Ok([]),
                          );
                     switch (fields) {
                     | Error(error) => Error(error)
                     | Ok(fields) =>
                       Ok([
                         Scheme.Collection({
                           collection,
                           fields,
                           input_type,
                           output_type: input_type,
                           validator:
                             switch (
                               validators_record.fields
                               |> ValidatorsRecordParser.collection(
                                    collection,
                                  )
                             ) {
                             | Ok(res) => Ok(res)
                             | Error(_) => Error()
                             },
                         }),
                         ...scheme,
                       ])
                     };
                   }
                 },
                 Ok([]),
               );
          | Record({entries: output_entries, loc: output_loc}) =>
            let validator =
                (
                  ~input_field: InputField.validated,
                  ~input_field_data: InputFieldData.validated,
                  ~output_field_data: OutputFieldData.t,
                  ~input_entries: list(InputTypeParser.validated_entry),
                  ~validators_record: ValidatorsRecord.t,
                )
                : result(FieldValidator.t, error) =>
              switch (input_field_data.async) {
              | None =>
                switch (
                  input_field |> InputTypeParser.in_deps_of(input_entries)
                ) {
                | Some(in_deps_of_field) =>
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
                  if (ItemType.eq(input_field_data.typ, output_field_data.typ)) {
                    switch (
                      validators_record.fields
                      |> ValidatorsRecordParser.optional(input_field)
                    ) {
                    | Ok(res) => Ok(SyncValidator(Ok(Optional(res))))
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
                                input_field_data.typ,
                                output_field_data.typ,
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
                      output_field_data.typ |> FieldOptionalityParser.parse,
                  }),
                )
              };

            let (
              result,
              input_fields_not_in_output,
              output_fields_not_in_input,
            ) =
              validated_input_entries
              |> List.rev
              |> List.fold_left(
                   (
                     (
                       result: result(Scheme.t, error),
                       input_fields_not_in_output: list(InputField.validated),
                       output_fields_not_in_input: list(OutputField.t),
                     ),
                     input_entry: InputTypeParser.validated_entry,
                   ) =>
                     switch (input_entry) {
                     | ValidatedInputField(input_field_data) =>
                       let output_field_data =
                         output_entries
                         |> List.fold_left(
                              (res, output_entry: OutputTypeParser.entry) =>
                                switch (res, output_entry) {
                                | (Some(_), _) => res
                                | (None, OutputField(output_field_data)) =>
                                  input_field_data.name
                                  == output_field_data.name
                                    ? Some(output_field_data) : None
                                | (None, OutputCollection(_)) => None
                                },
                              None,
                            );
                       switch (result, output_field_data) {
                       | (_, None) => (
                           result,
                           [
                             ValidatedInputField(input_field_data),
                             ...input_fields_not_in_output,
                           ],
                           output_fields_not_in_input,
                         )

                       | (Error(error), Some(output_field_data)) => (
                           Error(error),
                           input_fields_not_in_output,
                           output_fields_not_in_input
                           |> List.filter((output_field: OutputField.t) =>
                                switch (output_field) {
                                | OutputField(output_field_data) =>
                                  output_field_data.name
                                  != input_field_data.name
                                | OutputFieldOfCollection(_) => true
                                }
                              ),
                         )

                       | (Ok(scheme), Some(output_field_data)) =>
                         let validator =
                           validator(
                             ~input_field=
                               ValidatedInputField(input_field_data),
                             ~input_field_data,
                             ~output_field_data,
                             ~input_entries=validated_input_entries,
                             ~validators_record,
                           );
                         (
                           switch (validator) {
                           | Error(error) => Error(error)
                           | Ok(validator) =>
                             Ok([
                               Scheme.Field({
                                 name: input_field_data.name,
                                 input_type: input_field_data.typ,
                                 output_type: output_field_data.typ,
                                 validator,
                                 deps: input_field_data.deps,
                               }),
                               ...scheme,
                             ])
                           },
                           input_fields_not_in_output,
                           output_fields_not_in_input
                           |> List.filter((output_field: OutputField.t) =>
                                switch (output_field) {
                                | OutputField(output_field_data) =>
                                  output_field_data.name
                                  != input_field_data.name
                                | OutputFieldOfCollection(_) => true
                                }
                              ),
                         );
                       };

                     | ValidatedInputCollection({
                         collection: input_collection,
                         fields: input_fields,
                         input_type: input_collection_type,
                       }) =>
                       let output_collection =
                         output_entries
                         |> List.fold_left(
                              (res, output_entry: OutputTypeParser.entry) =>
                                switch (res, output_entry) {
                                | (Some(_), _) => res
                                | (None, OutputField(_)) => res
                                | (
                                    None,
                                    OutputCollection({
                                      collection: output_collection,
                                      fields,
                                      output_type,
                                    }),
                                  ) =>
                                  if (output_collection.plural
                                      == input_collection.plural) {
                                    Some((
                                      output_collection,
                                      fields,
                                      output_type,
                                    ));
                                  } else {
                                    None;
                                  }
                                },
                              None,
                            );
                       switch (output_collection) {
                       | None => (
                           Error(
                             OutputTypeParseError(
                               OutputCollectionNotFound({
                                 input_collection,
                                 loc: output_loc,
                               }),
                             ),
                           ),
                           input_fields
                           |> List.fold_left(
                                (acc: list(InputField.validated), field) =>
                                  [
                                    ValidatedInputFieldOfCollection({
                                      collection: input_collection,
                                      field,
                                    }),
                                    ...acc,
                                  ],
                                input_fields_not_in_output,
                              ),
                           output_fields_not_in_input,
                         )
                       | Some((output_collection, output_fields, output_type)) =>
                         let (
                           fields,
                           input_fields_not_in_output,
                           output_fields_not_in_input,
                         ) =
                           input_fields
                           |> List.rev
                           |> List.fold_left(
                                (
                                  (
                                    res: result(list(Scheme.field), error),
                                    input_fields_not_in_output:
                                      list(InputField.validated),
                                    output_fields_not_in_input:
                                      list(OutputField.t),
                                  ),
                                  input_field_data: InputFieldData.validated,
                                ) => {
                                  let output_field_data =
                                    output_fields
                                    |> List.find_opt(
                                         (
                                           output_field_data: OutputFieldData.t,
                                         ) =>
                                         output_field_data.name
                                         == input_field_data.name
                                       );

                                  switch (res, output_field_data) {
                                  | (_, None) => (
                                      res,
                                      [
                                        ValidatedInputFieldOfCollection({
                                          collection: input_collection,
                                          field: input_field_data,
                                        }),
                                        ...input_fields_not_in_output,
                                      ],
                                      output_fields_not_in_input,
                                    )

                                  | (Error(error), Some(output_field_data)) => (
                                      Error(error),
                                      input_fields_not_in_output,
                                      output_fields_not_in_input
                                      |> List.filter(
                                           (output_field: OutputField.t) =>
                                           switch (output_field) {
                                           | OutputField(_) => true
                                           | OutputFieldOfCollection({
                                               collection,
                                               field,
                                             }) =>
                                             !(
                                               input_collection.plural
                                               == collection.plural
                                               && output_field_data.name
                                               == field.name
                                             )
                                           }
                                         ),
                                    )
                                  | (Ok(fields), Some(output_field_data)) =>
                                    let validator =
                                      validator(
                                        ~input_field=
                                          ValidatedInputFieldOfCollection({
                                            collection: input_collection,
                                            field: input_field_data,
                                          }),
                                        ~input_field_data,
                                        ~output_field_data,
                                        ~input_entries=validated_input_entries,
                                        ~validators_record,
                                      );
                                    (
                                      switch (validator) {
                                      | Error(error) => Error(error)
                                      | Ok(validator) =>
                                        Ok([
                                          {
                                            name: input_field_data.name,
                                            input_type: input_field_data.typ,
                                            output_type: output_field_data.typ,
                                            validator,
                                            deps: input_field_data.deps,
                                          },
                                          ...fields,
                                        ])
                                      },
                                      input_fields_not_in_output,
                                      output_fields_not_in_input
                                      |> List.filter(
                                           (output_field: OutputField.t) =>
                                           switch (output_field) {
                                           | OutputField(_) => true
                                           | OutputFieldOfCollection({
                                               collection,
                                               field,
                                             }) =>
                                             !(
                                               input_collection.plural
                                               == collection.plural
                                               && output_field_data.name
                                               == field.name
                                             )
                                           }
                                         ),
                                    );
                                  };
                                },
                                (
                                  Ok([]),
                                  input_fields_not_in_output,
                                  output_fields_not_in_input,
                                ),
                              );

                         switch (result, fields) {
                         | (Error(error), _) => (
                             result,
                             input_fields_not_in_output,
                             output_fields_not_in_input,
                           )
                         | (Ok(_), Error(error)) => (
                             Error(error),
                             input_fields_not_in_output,
                             output_fields_not_in_input,
                           )
                         | (Ok(scheme), Ok(fields)) => (
                             Ok([
                               Scheme.Collection({
                                 collection: input_collection,
                                 fields,
                                 input_type: input_collection_type,
                                 output_type,
                                 validator:
                                   switch (
                                     validators_record.fields
                                     |> ValidatorsRecordParser.collection(
                                          input_collection,
                                        )
                                   ) {
                                   | Ok(res) => Ok(res)
                                   | Error(_) => Error()
                                   },
                               }),
                               ...scheme,
                             ]),
                             input_fields_not_in_output,
                             output_fields_not_in_input,
                           )
                         };
                       };
                     },
                   (Ok([]), [], output_entries |> OutputTypeParser.flatten),
                 );
            switch (input_fields_not_in_output, output_fields_not_in_input) {
            | ([], []) => result
            | (input_fields_not_in_output, []) =>
              Error(
                IOMismatch(
                  InputFieldsNotInOutput({
                    fields: input_fields_not_in_output,
                    loc: output_loc,
                  }),
                ),
              )
            | ([], output_entries_not_in_input) =>
              Error(
                IOMismatch(
                  OutputFieldsNotInInput({
                    fields: output_fields_not_in_input,
                  }),
                ),
              )
            | (input_fields_not_in_output, output_fields_not_in_input) =>
              Error(
                IOMismatch(
                  Both({
                    input_fields_not_in_output,
                    output_fields_not_in_input,
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
                   | Collection({fields}) =>
                     fields
                     |> List.exists((field: Scheme.field) =>
                          switch (field) {
                          | {validator: AsyncValidator(_)} => true
                          | {validator: SyncValidator(_)} => false
                          }
                        )
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
