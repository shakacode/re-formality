let takenEmail = "test@taken.email"

let validateEmail = value =>
  Js.Promise.make((~resolve, ~reject as _) => {
    Js.log("Remote validation triggered")
    Js.Global.setTimeout(() =>
      value !== takenEmail
        ? {
            Js.log("Remote validation succeeded")
            resolve(true)
          }
        : {
            Js.log("Remote validation failed")
            resolve(false)
          }
    , 1500)->ignore
  })
