module Form = [%form
  type input = {name: string};
  type message = int;
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name}) => Ok(name),
    },
  }
];
