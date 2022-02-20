@react.component
let make = (~className, ~onSubmit, ~children) =>
  <form
    className
    onSubmit={event => {
      if !(event->ReactEvent.Form.defaultPrevented) {
        event->ReactEvent.Form.preventDefault
      }
      onSubmit()
    }}>
    children
  </form>
