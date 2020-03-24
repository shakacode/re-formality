module Form = [%form
  {target: ReactNative};
  type input = {name: string};
  let validators = {
    name: {
      strategy: OnSubmit,
      validate: ({name}) => Ok(name),
    },
  }
];
