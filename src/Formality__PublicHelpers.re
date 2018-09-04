let emptyString = value => value == "";

module Dom = {
  let toValueOnChange = event => event->ReactEvent.Form.target##value;

  let toValueOnBlur = event => event->ReactEvent.Focus.target##value;

  let toCheckedOnChange = event => event->ReactEvent.Form.target##checked;

  let toCheckedOnBlur = event => event->ReactEvent.Focus.target##checked;

  let preventDefault = (submit, event) => {
    if (!event->ReactEvent.Form.defaultPrevented) {
      event->ReactEvent.Form.preventDefault;
    };
    submit();
  };
};
