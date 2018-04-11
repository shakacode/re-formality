let emptyString = value => value == "";

module Dom = {
  let targetValue = element : string => (element |> ReactDOMRe.domElementToObj)##value;
  let toValueOnChange = event =>
    event |> ReactEventRe.Form.target |> targetValue;
  let toValueOnBlur = event =>
    event |> ReactEventRe.Focus.target |> targetValue;
  let preventDefault = (submit, event) => {
    if (! (event |> ReactEventRe.Form.defaultPrevented)) {
      event |> ReactEventRe.Form.preventDefault;
    };
    submit();
  };
};
