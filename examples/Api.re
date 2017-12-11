let takenEmail = "test@taken.email";

let validateEmail = value =>
  Js.Promise.make((~resolve, ~reject as _) => {
    Js.log("Remote validation triggered");
    Js.Global.setTimeout(
      () =>
        value !== takenEmail ?
          {
            Js.log("Remote validation succeeded");
            [@bs] resolve(true);
          } :
          {
            Js.log("Remote validation failed");
            [@bs] resolve(false);
          },
      1500
    )
    |> ignore;
  });
