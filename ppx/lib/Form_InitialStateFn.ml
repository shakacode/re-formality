open Ppxlib

let ast ~loc =
  [%stri
    let initialState =
      [%e
        Uncurried.fn
          ~loc
          ~arity:1
          [%expr
            fun input ->
              { input
              ; fieldsStatuses = initialFieldsStatuses input [@res.uapp]
              ; collectionsStatuses = initialCollectionsStatuses
              ; formStatus = Editing
              ; submissionStatus = NeverSubmitted
              }]]
    ;;]
;;
