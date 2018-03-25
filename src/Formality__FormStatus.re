type t('field, 'message) =
  | Editing
  | Submitting
  | Submitted
  | SubmissionFailed(list(('field, 'message)), option('message));
