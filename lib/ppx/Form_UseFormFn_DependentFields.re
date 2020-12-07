open Meta;
open Ast;
open AstHelpers;

open Ppxlib;
open Ast_helper;

let ast =
    (
      ~loc,
      ~dep: FieldDep.t,
      ~deps: list(FieldDep.t),
      ~trigger: [
         | `Field(string)
         | `Collection(Collection.t)
         | `FieldOfCollection(Collection.t, string)
       ],
      ~metadata: option(unit),
      scheme: Scheme.t,
    ) => {
  let validate_dep = (dep: FieldDep.t) => {
    switch (
      scheme
      |> List.fold_left(
           (res, entry: Scheme.entry) =>
             switch (res, entry, dep) {
             | (Some(_), _, _) => res
             | (None, Field(field), DepField(dep)) =>
               field.name == dep ? Some(`DepField(field)) : None
             | (
                 None,
                 Collection({collection, fields}),
                 DepFieldOfCollection({
                   collection: dep_collection,
                   field: dep_field,
                 }),
               ) =>
               if (collection.plural != dep_collection.plural) {
                 None;
               } else {
                 Some(
                   `DepFieldOfCollection((
                     collection,
                     fields
                     |> List.find((field: Scheme.field) =>
                          field.name == dep_field
                        ),
                   )),
                 );
               }
             | (None, Collection(_), DepField(_))
             | (None, Field(_), DepFieldOfCollection(_)) => res
             },
           None,
         )
    ) {
    | None =>
      failwith(
        "Dep is not found in scheme. Please, file an issue with your use-case.",
      )
    | Some(`DepField(field)) =>
      let field_status_expr =
        field.name |> E.ref_field(~in_="nextFieldsStatuses", ~loc);
      let validator_expr = field.name |> E.field(~in_="validators", ~loc);
      let set_status_expr =
        field.name
        |> E.update_ref_field(
             ~in_="nextFieldsStatuses",
             ~with_=[%expr status],
             ~loc,
           );

      switch (field.validator) {
      | SyncValidator(Ok(Required | Optional(Some(_))) | Error ()) =>
        switch%expr (
          switch%e (metadata) {
          | None =>
            %expr
            validateDependentFieldOnChange(
              ~input=nextInput,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            )
          | Some () =>
            %expr
            validateDependentFieldOnChangeWithMetadata(
              ~input=nextInput,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~metadata,
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            )
          }
        ) {
        | Some(result) => nextFieldsStatuses := result
        | None => ()
        }
      | SyncValidator(Ok(Optional(None))) =>
        %expr
        ()
      // Should we trigger async validator of dependency?
      | AsyncValidator({mode: OnChange | OnBlur}) =>
        switch%expr (
          switch%e (metadata) {
          | None =>
            %expr
            Async.validateDependentFieldOnChange(
              ~input=nextInput,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            )
          | Some () =>
            %expr
            Async.validateDependentFieldOnChangeWithMetadata(
              ~input=nextInput,
              ~fieldStatus=[%e field_status_expr],
              ~validator=[%e validator_expr],
              ~metadata,
              ~setStatus=[%e [%expr status => [%e set_status_expr]]],
            )
          }
        ) {
        | Some(result) => nextFieldsStatuses := result
        | None => ()
        }
      };
    | Some(`DepFieldOfCollection(collection, field)) =>
      let collection_statuses_expr =
        collection.plural |> E.ref_field(~in_="nextFieldsStatuses", ~loc);
      let field_status_expr = field.name |> E.field(~in_="item", ~loc);
      let validator_expr =
        field.name
        |> E.field_of_collection_validator(
             ~validators="validators",
             ~collection,
             ~loc,
           );
      let set_status_expr =
        field.name
        |> E.update_ref_field_of_collection(
             ~in_="nextFieldsStatuses",
             ~collection,
             ~with_=[%expr status],
             ~index_token="index'",
             ~loc,
           );

      switch (trigger) {
      | `FieldOfCollection(collection', field')
          when collection.plural == collection'.plural && field.name == field' =>
        switch (field.validator) {
        | SyncValidator(Ok(Required | Optional(Some(_))) | Error ()) =>
          %expr
          {
            Belt.Array.forEachWithIndex(
              [%e collection_statuses_expr], (index', item) =>
              if (index != index') {
                switch (
                  switch%e (metadata) {
                  | None =>
                    %expr
                    validateDependentFieldOfCollectionOnChange(
                      ~input=nextInput,
                      ~index=index',
                      ~fieldStatus=[%e field_status_expr],
                      ~validator=[%e validator_expr],
                      ~setStatus=[%e [%expr status => [%e set_status_expr]]],
                    )
                  | Some () =>
                    %expr
                    validateDependentFieldOfCollectionOnChangeWithMetadata(
                      ~input=nextInput,
                      ~index=index',
                      ~fieldStatus=[%e field_status_expr],
                      ~validator=[%e validator_expr],
                      ~metadata,
                      ~setStatus=[%e [%expr status => [%e set_status_expr]]],
                    )
                  }
                ) {
                | Some(result) => nextFieldsStatuses := result
                | None => ()
                };
              } else {
                ();
              }
            );
          }

        | SyncValidator(Ok(Optional(None))) =>
          %expr
          ()
        // Should we trigger async validator of dependency?
        | AsyncValidator({mode: OnChange | OnBlur}) =>
          %expr
          {
            Belt.Array.forEachWithIndex(
              [%e collection_statuses_expr], (index', item) =>
              if (index != index') {
                switch (
                  switch%e (metadata) {
                  | None =>
                    %expr
                    Async.validateDependentFieldOfCollectionOnChange(
                      ~input=nextInput,
                      ~index=index',
                      ~fieldStatus=[%e field_status_expr],
                      ~validator=[%e validator_expr],
                      ~setStatus=[%e [%expr status => [%e set_status_expr]]],
                    )
                  | Some () =>
                    %expr
                    Async.validateDependentFieldOfCollectionOnChangeWithMetadata(
                      ~input=nextInput,
                      ~index=index',
                      ~fieldStatus=[%e field_status_expr],
                      ~validator=[%e validator_expr],
                      ~metadata,
                      ~setStatus=[%e [%expr status => [%e set_status_expr]]],
                    )
                  }
                ) {
                | Some(result) => nextFieldsStatuses := result
                | None => ()
                };
              } else {
                ();
              }
            );
          }
        }
      | `Field(_)
      | `Collection(_)
      | `FieldOfCollection(_, _) =>
        switch (field.validator) {
        | SyncValidator(Ok(Required | Optional(Some(_))) | Error ()) =>
          %expr
          {
            Belt.Array.forEachWithIndex(
              [%e collection_statuses_expr], (index', item) =>
              switch (
                switch%e (metadata) {
                | None =>
                  %expr
                  validateDependentFieldOfCollectionOnChange(
                    ~input=nextInput,
                    ~index=index',
                    ~fieldStatus=[%e field_status_expr],
                    ~validator=[%e validator_expr],
                    ~setStatus=[%e [%expr status => [%e set_status_expr]]],
                  )
                | Some () =>
                  %expr
                  validateDependentFieldOfCollectionOnChangeWithMetadata(
                    ~input=nextInput,
                    ~index=index',
                    ~fieldStatus=[%e field_status_expr],
                    ~validator=[%e validator_expr],
                    ~metadata,
                    ~setStatus=[%e [%expr status => [%e set_status_expr]]],
                  )
                }
              ) {
              | Some(result) => nextFieldsStatuses := result
              | None => ()
              }
            );
          }
        | SyncValidator(Ok(Optional(None))) =>
          %expr
          ()
        // Should we trigger async validator of dependency?
        | AsyncValidator({mode: OnChange | OnBlur}) =>
          %expr
          {
            Belt.Array.forEachWithIndex(
              [%e collection_statuses_expr], (index', item) =>
              switch (
                switch%e (metadata) {
                | None =>
                  %expr
                  Async.validateDependentFieldOfCollectionOnChange(
                    ~input=nextInput,
                    ~index=index',
                    ~fieldStatus=[%e field_status_expr],
                    ~validator=[%e validator_expr],
                    ~setStatus=[%e [%expr status => [%e set_status_expr]]],
                  )
                | Some() =>
                  %expr
                  Async.validateDependentFieldOfCollectionOnChangeWithMetadata(
                    ~input=nextInput,
                    ~index=index',
                    ~fieldStatus=[%e field_status_expr],
                    ~validator=[%e validator_expr],
                    ~metadata,
                    ~setStatus=[%e [%expr status => [%e set_status_expr]]],
                  )
                }
              ) {
              | Some(result) => nextFieldsStatuses := result
              | None => ()
              }
            );
          }
        }
      };
    };
  };

  deps |> E.seq(~exp=dep |> validate_dep, ~make=validate_dep);
};
