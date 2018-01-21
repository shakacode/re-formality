let comparator = (a, b) => a === b ? 0 : 1;

let targetValue = element => (element |> ReactDOMRe.domElementToObj)##value;

let formEventTargetValue = event =>
  event |> ReactEventRe.Form.target |> targetValue;

let focusEventTargetValue = event =>
  event |> ReactEventRe.Focus.target |> targetValue;
