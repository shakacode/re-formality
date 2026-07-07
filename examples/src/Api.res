let takenEmail = "test@taken.email"

let validateEmail = value =>
  Promise.make((resolve, _) => {
    Console.log("Remote validation triggered")
    setTimeout(() =>
      value !== takenEmail
        ? {
            Console.log("Remote validation succeeded")
            resolve(true)
          }
        : {
            Console.log("Remote validation failed")
            resolve(false)
          }
    , 1500)->ignore
  })
