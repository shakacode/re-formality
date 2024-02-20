open Ppxlib
let ast ~loc  =
  [%stri
    let initialState input =
      {
        input;
        fieldsStatuses = (input |. initialFieldsStatuses);
        collectionsStatuses = initialCollectionsStatuses;
        formStatus = Editing;
        submissionStatus = NeverSubmitted
      }]