open Ppxlib;

let ast = (~loc) => [%stri
  type state = {
    input,
    fieldsStatuses,
    collectionsStatuses,
    formStatus: formStatus(submissionError),
    submissionStatus,
  }
];
