open Ppxlib

let ast ~loc =
  [%stri
    type state =
      { input : input
      ; fieldsStatuses : fieldsStatuses
      ; collectionsStatuses : collectionsStatuses
      ; formStatus : submissionError formStatus
      ; submissionStatus : submissionStatus
      }]
;;
