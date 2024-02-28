open Meta
open Ppxlib
open Ast_helper

let ext =
  Extension.declare
    "form"
    Extension.Context.module_expr
    Ast_pattern.__
    (fun ~loc ~path:_ expr ->
      match expr with
      | PStr structure ->
        (match structure |> Metadata.make with
         | Ok
             { scheme
             ; async
             ; output_type
             ; message_type
             ; submission_error_type
             ; validators_record
             ; metadata
             ; debounce_interval
             } ->
           let head = [ Form_Attributes.ast ~loc; Form_OpenFormality.ast ~loc ] in
           let types =
             let types =
               ref
                 [ Form_FieldsStatusesType.ast ~scheme ~loc
                 ; Form_CollectionsStatusesType.ast ~scheme ~loc
                 ; Form_StateType.ast ~loc
                 ; Form_ActionType.ast ~scheme ~loc
                 ; Form_ValidatorsType.ast ~scheme ~metadata ~loc
                 ; Form_InterfaceType.ast ~scheme ~async ~loc
                 ]
             in
             (match submission_error_type with
              | None -> types := SubmissionErrorType.default ~loc :: !types
              | Some () -> ());
             (match message_type with
              | None -> types := MessageType.default ~loc :: !types
              | Some () -> ());
             (match output_type with
              | NotProvided -> types := OutputType.default ~loc :: !types
              | AliasOfInput | Record _ -> ());
             !types
           in
           let values =
             let values = ref [] in
             (match debounce_interval with
              | None when async -> values := DebounceInterval.default ~loc :: !values
              | None | Some () -> ());
             !values
           in
           let funcs =
             [ Form_InitialFieldsStatusesFn.ast ~scheme ~loc
             ; Form_InitialCollectionsStatuses.ast ~scheme ~loc
             ; Form_InitialStateFn.ast ~loc
             ; (match async with
                | true -> Form_ValidateFormFn.Async.ast ~scheme ~metadata ~loc
                | false -> Form_ValidateFormFn.Sync.ast ~scheme ~metadata ~loc)
             ; Form_UseFormFn.ast ~scheme ~async ~metadata ~loc
             ]
           in
           let structure =
             (structure
              |> List.rev
              |> List.fold_left
                   (fun acc (structure_item : structure_item) ->
                     match structure_item with
                     | { pstr_desc = Pstr_value (rec_flag, value_bindings); pstr_loc } ->
                       let value_bindings, search_result =
                         value_bindings
                         |> List.rev
                         |> List.fold_left
                              (fun (acc, (res : [ `Found | `NotFound ])) value ->
                                match value with
                                | { pvb_pat =
                                      { ppat_desc = Ppat_var { txt = "validators" } }
                                  } as value ->
                                  ( (value
                                     |> Form_ValidatorsRecord.ast
                                          ~scheme
                                          ~metadata
                                          ~validators:validators_record)
                                    :: acc
                                  , `Found )
                                | _ as value -> value :: acc, res)
                              ([], `NotFound)
                       in
                       let structure_item =
                         { pstr_desc = Pstr_value (rec_flag, value_bindings); pstr_loc }
                       in
                       (match search_result with
                        | `NotFound -> structure_item :: acc
                        | `Found ->
                          List.rev_append
                            (types |> List.rev |> List.rev_append values)
                            (structure_item :: acc))
                     | _ -> structure_item :: acc)
                   funcs
               : structure)
           in
           Mod.mk (Pmod_structure (structure |> List.rev_append (head |> List.rev)))
         | Error (InputTypeParseError NotFound) ->
           Location.raise_errorf ~loc "`input` type not found"
         | Error (InputTypeParseError (NotRecord loc)) ->
           Location.raise_errorf ~loc "`input` must be of record type"
         | Error
             (InputTypeParseError
               (InvalidAttributes
                 ( InvalidAsyncField (InvalidPayload loc)
                 | InvalidCollectionField (InvalidAsyncField (InvalidPayload loc)) ))) ->
           Location.raise_errorf
             ~loc
             "`@field.async` attribute accepts only optional record `{mode: OnChange | \
              OnBlur}`"
         | Error
             (InputTypeParseError
               (InvalidAttributes
                 ( InvalidAsyncField (InvalidAsyncMode loc)
                 | InvalidCollectionField (InvalidAsyncField (InvalidAsyncMode loc)) )))
           ->
           Location.raise_errorf
             ~loc
             "Invalid async mode. Use either `OnChange` or `OnBlur`."
         | Error
             (InputTypeParseError
               (InvalidAttributes
                 ( InvalidFieldDeps (DepsParseError loc)
                 | InvalidCollectionField (InvalidFieldDeps (DepsParseError loc)) ))) ->
           Location.raise_errorf
             ~loc
             "`@field.deps` attribute must contain field or tuple of fields"
         | Error
             (InputTypeParseError
               (InvalidAttributes
                 ( InvalidFieldDeps (DepNotFound dep)
                 | InvalidCollectionField (InvalidFieldDeps (DepNotFound dep)) ))) ->
           let field, loc =
             match dep with
             | UnvalidatedDepField { name; loc } -> name, loc
             | UnvalidatedDepFieldOfCollection { collection; field; f_loc } ->
               collection ^ "." ^ field, f_loc
           in
           Location.raise_errorf ~loc "Field `%s` doesn't exist in input" field
         | Error
             (InputTypeParseError
               (InvalidAttributes
                 ( InvalidFieldDeps (DepOfItself (`Field (name, loc)))
                 | InvalidCollectionField
                     (InvalidFieldDeps (DepOfItself (`Field (name, loc)))) ))) ->
           Location.raise_errorf ~loc "Field `%s` depends on itself" name
         | Error
             (InputTypeParseError
               (InvalidAttributes
                 ( InvalidFieldDeps (DepDuplicate dep)
                 | InvalidCollectionField (InvalidFieldDeps (DepDuplicate dep)) ))) ->
           let field, loc =
             match dep with
             | UnvalidatedDepField { name; loc } -> name, loc
             | UnvalidatedDepFieldOfCollection { collection; field; f_loc } ->
               collection ^ "." ^ field, f_loc
           in
           Location.raise_errorf
             ~loc
             "Field `%s` is already declared as a dependency for this field"
             field
         | Error
             (InputTypeParseError
               (InvalidAttributes (Conflict (`AsyncWithCollection loc)))) ->
           Location.raise_errorf
             ~loc
             "Collection can not be `async`. If you want to make specific fields in \
              collection async, you can apply `field.async` attribute to specific \
              fields."
         | Error
             (InputTypeParseError
               (InvalidAttributes (Conflict (`DepsWithCollection loc)))) ->
           Location.raise_errorf ~loc "Collection can not have deps"
         | Error
             (InputTypeParseError
               (InvalidAttributes (InvalidCollectionField (NotArray loc)))) ->
           Location.raise_errorf ~loc "Collection must be an array of records"
         | Error
             (InputTypeParseError
               (InvalidAttributes (InvalidCollectionField (InvalidTypeRef loc)))) ->
           Location.raise_errorf
             ~loc
             "Collection must be an array of records. Record type of collection entry \
              must be defined within this module."
         | Error
             (InputTypeParseError
               (InvalidAttributes (InvalidCollectionField (RecordNotFound loc)))) ->
           Location.raise_errorf
             ~loc
             "This type can not be found. Record type of collection entry must be \
              defined within this module."
         | Error
             (InputTypeParseError
               (InvalidAttributes (InvalidCollectionField (NotRecord loc)))) ->
           Location.raise_errorf ~loc "Type of collection entry must be a record"
         | Error (OutputTypeParseError (NotRecord loc)) ->
           Location.raise_errorf
             ~loc
             "`output` must be of record type or an alias of `input`"
         | Error (OutputTypeParseError (BadTypeAlias { alias = _; loc })) ->
           Location.raise_errorf
             ~loc
             "`output` can only be an alias of `input` type or a record"
         | Error (OutputTypeParseError (InputNotAvailable loc)) ->
           Location.raise_errorf ~loc "`input` type is not found or in invalid state"
         | Error
             (OutputTypeParseError (OutputCollectionNotFound { input_collection; loc }))
           ->
           Location.raise_errorf
             ~loc
             "`output` type for %s collection that is defined in `input` is not found or \
              in invalid state"
             input_collection.plural
         | Error
             (OutputTypeParseError
               (InvalidCollection
                 ( InvalidCollectionTypeRef loc
                 | CollectionTypeNotRecord loc
                 | CollectionTypeNotFound loc
                 | CollectionOutputNotArray loc ))) ->
           Location.raise_errorf
             ~loc
             "Collection must be an array of records. Record type of collection entry \
              must be defined within this module."
         | Error (ValidatorsRecordParseError NotFound) ->
           Location.raise_errorf ~loc "`validators` record not found"
         | Error (ValidatorsRecordParseError (NotRecord loc | RecordParseError loc)) ->
           Location.raise_errorf
             ~loc
             "Failed to parse `validators` record. Please, file an issue with your \
              use-case."
         | Error (ValidatorsRecordParseError (BadTypeAnnotation loc)) ->
           Location.raise_errorf
             ~loc
             "`validators` binding must be of `validators` type. You can safely remove \
              type annotation and it will be annotated for you under the hood."
         | Error
             (ValidatorsRecordParseError
               (ValidatorError
                 (`BadRequiredValidator (field, (`Some loc | `None loc), reason)))) ->
           (match reason with
            | `DifferentIO (_input_type, _output_type) ->
              Location.raise_errorf
                ~loc
                "Validator for `%s` field is required because its input and output types \
                 are different. So validator function is required to produce value of \
                 output type from an input value."
                (match field with
                 | ValidatedInputField field -> field.name
                 | ValidatedInputFieldOfCollection { collection; field } ->
                   collection.singular ^ "." ^ field.name)
            | `IncludedInDeps in_deps_of_field ->
              Location.raise_errorf
                ~loc
                "Validator for `%s` field is required because this field is included in \
                 deps of `%s` field"
                (match field with
                 | ValidatedInputField field -> field.name
                 | ValidatedInputFieldOfCollection { collection; field } ->
                   collection.singular ^ "." ^ field.name)
                (match in_deps_of_field with
                 | ValidatedInputField field -> field.name
                 | ValidatedInputFieldOfCollection { collection; field } ->
                   collection.singular ^ "." ^ field.name))
         | Error (IOMismatch (OutputFieldsNotInInput { fields })) ->
           (match fields with
            | [] ->
              failwith
                "Empty list of non-matched fields in IOMatchError(OutputFieldsNotInInput)"
            | field :: [] | field :: _ ->
              Location.raise_errorf
                ~loc
                "`output` field `%s` doesn't exist in `input` type"
                (match field with
                 | OutputField field -> field.name
                 | OutputFieldOfCollection { collection; field } ->
                   collection.singular ^ "." ^ field.name))
         | Error (IOMismatch (InputFieldsNotInOutput { fields; loc }))
         | Error
             (IOMismatch
               (Both
                 { input_fields_not_in_output = fields
                 ; output_fields_not_in_input = _
                 ; loc
                 })) ->
           (match fields with
            | [] -> failwith "Empty list of non-matched fields in IOMatchError(Both)"
            | field :: [] ->
              Location.raise_errorf
                ~loc
                "`input` field `%s` doesn't exist in `output` type"
                (match field with
                 | ValidatedInputField field -> field.name
                 | ValidatedInputFieldOfCollection { collection; field } ->
                   collection.singular ^ "." ^ field.name)
            | fields ->
              Location.raise_errorf
                ~loc
                "Some `input` fields don't exist in `output` type: %s"
                (fields
                 |> List.rev
                 |> List.rev_map (fun (field : InputField.validated) ->
                   match field with
                   | ValidatedInputField field -> field.name
                   | ValidatedInputFieldOfCollection { collection; field } ->
                     collection.singular ^ "." ^ field.name)
                 |> String.concat ", ")))
      | _ -> Location.raise_errorf ~loc "Must be a structure")
;;
