type t('submissionError) =
  | Editing
  | Submitting
  | Submitted
  | SubmissionFailed('submissionError);
