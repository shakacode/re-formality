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

module LoginFormNoId = Formality.Make(LoginForm);

module LoginFormWithId =
  Formality.MakeId({
    include LoginForm;
    module FieldId =
      Id.MakeComparable({
        type t = field;
        let cmp = Pervasives.compare;
      });
  });

module LoginFormAsyncNoId =
  Formality.Async.Make({
    include LoginForm;
    let debounceInterval = 100;
  });

module LoginFormAsyncId =
  Formality.Async.MakeId({
    include LoginForm;
    let debounceInterval = 100;
    module FieldId =
      Id.MakeComparable({
        type t = field;
        let cmp = Pervasives.compare;
      });
  });

module LoginFormAsyncBlurNoId =
  Formality.Async.MakeOnBlur(LoginForm);


module LoginFormAsyncBlurId =
  Formality.Async.MakeOnBlurId({
  include LoginForm;
   module FieldId =
        Id.MakeComparable({
          type t = field;
          let cmp = Pervasives.compare;
        });
  });
