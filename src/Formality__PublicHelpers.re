let emptyString = value => value === "";

module Dom = {
  let valueOnChange = (handle, event) =>
    event |> ReactEventRe.Form.target |> Formality__Utils.targetValue |> handle;
  let valueOnBlur = (handle, event) =>
    event
    |> ReactEventRe.Focus.target
    |> Formality__Utils.targetValue
    |> handle;
  let preventDefault = (submit, event) => {
    if (! (event |> ReactEventRe.Form.defaultPrevented)) {
      event |> ReactEventRe.Form.preventDefault;
    };
    submit();
  };
};
