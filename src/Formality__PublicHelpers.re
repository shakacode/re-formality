let emptyString = value => value == "";

module Dom = {
  let targetValue = element : string => (element |> ReactDOMRe.domElementToObj)##value;
  let targetChecked = element : bool => (element |> ReactDOMRe.domElementToObj)##checked;

  let toValueOnChange = event =>
    event |> ReactEventRe.Form.target |> targetValue;

  let toValueOnBlur = event =>
    event |> ReactEventRe.Focus.target |> targetValue;

  let toCheckedOnChange = event =>
    event |> ReactEventRe.Form.target |> targetChecked;

  let toCheckedOnBlur = event =>
    event |> ReactEventRe.Focus.target |> targetChecked;

  let preventDefault = (submit, event) => {
    if (! (event |> ReactEventRe.Form.defaultPrevented)) {
      event |> ReactEventRe.Form.preventDefault;
    };
    submit();
  };
};
