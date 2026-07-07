open Meta

module FieldPrinter = struct
  let update_action ~field = "Update" ^ (field |> String.capitalize_ascii) ^ "Field"
  let blur_action ~field = "Blur" ^ (field |> String.capitalize_ascii) ^ "Field"

  let apply_async_result_action ~field =
    "ApplyAsyncResultFor" ^ (field |> String.capitalize_ascii) ^ "Field"
  ;;

  let update_fn ~field = "update" ^ (field |> String.capitalize_ascii)
  let blur_fn ~field = "blur" ^ (field |> String.capitalize_ascii)
  let result_value ~field = field ^ "Result"
end

module FieldOfCollectionPrinter = struct
  let update_action ~field ~(collection : Collection.t) =
    "Update"
    ^ (collection.singular |> String.capitalize_ascii)
    ^ (field |> String.capitalize_ascii)
    ^ "Field"
  ;;

  let blur_action ~field ~(collection : Collection.t) =
    "Blur"
    ^ (collection.singular |> String.capitalize_ascii)
    ^ (field |> String.capitalize_ascii)
    ^ "Field"
  ;;

  let apply_async_result_action ~field ~(collection : Collection.t) =
    "ApplyAsyncResultFor"
    ^ (collection.singular |> String.capitalize_ascii)
    ^ (field |> String.capitalize_ascii)
    ^ "Field"
  ;;

  let update_fn ~field ~(collection : Collection.t) =
    "update"
    ^ (collection.singular |> String.capitalize_ascii)
    ^ (field |> String.capitalize_ascii)
  ;;

  let blur_fn ~field ~(collection : Collection.t) =
    "blur"
    ^ (collection.singular |> String.capitalize_ascii)
    ^ (field |> String.capitalize_ascii)
  ;;

  let result_fn ~field ~(collection : Collection.t) =
    collection.singular ^ (field |> String.capitalize_ascii) ^ "Result"
  ;;
end

module CollectionPrinter = struct
  let fields_statuses_type (collection : Collection.t) =
    collection.singular ^ "FieldsStatuses"
  ;;

  let validator_type (collection : Collection.t) = collection.plural ^ "Validators"

  let add_action (collection : Collection.t) =
    "Add" ^ (collection.singular |> String.capitalize_ascii) ^ "Entry"
  ;;

  let remove_action (collection : Collection.t) =
    "Remove" ^ (collection.singular |> String.capitalize_ascii) ^ "Entry"
  ;;

  let add_fn (collection : Collection.t) =
    "add" ^ (collection.singular |> String.capitalize_ascii)
  ;;

  let remove_fn (collection : Collection.t) =
    "remove" ^ (collection.singular |> String.capitalize_ascii)
  ;;

  let result_value (collection : Collection.t) = collection.plural ^ "Result"
end
