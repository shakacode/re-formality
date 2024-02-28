module Dom: {
  let preventDefault: (unit => 'a, ReactEvent.Form.t) => 'a

  @deprecated("Use event->ReactEvent.Form.target##value instead.")
  let toValueOnChange: ReactEvent.Form.t => 'a

  @deprecated("Use event->ReactEvent.Focus.target##value instead.")
  let toValueOnBlur: ReactEvent.Focus.t => 'a

  @deprecated("Use event->ReactEvent.Form.target##checked instead.")
  let toCheckedOnChange: ReactEvent.Form.t => 'a

  @deprecated("Use event->ReactEvent.Focus.target##checked instead.")
  let toCheckedOnBlur: ReactEvent.Focus.t => 'a
}
