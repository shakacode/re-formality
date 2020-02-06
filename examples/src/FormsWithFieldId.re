module LoginForm = {
  type field =
    | Email
    | Password
    | RememberMe;

  type state = {
    email: string,
    password: string,
    rememberMe: bool,
  };

  type message = string;
  type submissionError = unit;

  let validators = [];
};

module LoginFormNoId = FormalityCompat.Make(LoginForm);

module LoginFormWithId =
  FormalityCompat.MakeWithId({
    include LoginForm;
    module FieldId =
      Id.MakeComparable({
        type t = field;
        let cmp = Pervasives.compare;
      });
  });

module LoginFormAsyncNoId =
  FormalityCompat.Async.Make({
    include LoginForm;
    let debounceInterval = 100;
  });

module LoginFormAsyncId =
  FormalityCompat.Async.MakeWithId({
    include LoginForm;
    let debounceInterval = 100;
    module FieldId =
      Id.MakeComparable({
        type t = field;
        let cmp = Pervasives.compare;
      });
  });

module LoginFormAsyncBlurNoId = FormalityCompat.Async.MakeOnBlur(LoginForm);

module LoginFormAsyncBlurId =
  FormalityCompat.Async.MakeOnBlurWithId({
    include LoginForm;
    module FieldId =
      Id.MakeComparable({
        type t = field;
        let cmp = Pervasives.compare;
      });
  });
