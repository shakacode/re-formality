switch ReactDOM.querySelector("#app") {
| Some(element) =>
  let root = element->ReactDOM.Client.createRoot
  root->ReactDOM.Client.Root.render(<App />)
| None => failwith("DOM node `#app` not found")
}
