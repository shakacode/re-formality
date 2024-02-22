@@ocaml.warning("-30")

type rec update<'state, 'action> =
  | NoUpdate
  | Update('state)
  | UpdateWithSideEffects('state, self<'state, 'action> => unit)
and dispatch<'action> = 'action => unit
and self<'state, 'action> = {
  state: 'state,
  dispatch: dispatch<'action>,
}
and fullState<'state, 'action> = {
  state: 'state,
  sideEffects: ref<array<self<'state, 'action> => unit>>,
}

type reducer<'state, 'action> = ('state, 'action) => update<'state, 'action>

let useReducer = (initialState: 'state, reducer: reducer<'state, 'action>) => {
  let ({state, sideEffects}, dispatch) = React.useReducer(
    ({state, sideEffects} as fullState, action) =>
      switch reducer(state, action) {
      | NoUpdate => fullState
      | Update(state) => {...fullState, state}
      | UpdateWithSideEffects(state, sideEffect) => {
          state,
          sideEffects: Array.concat(sideEffects.contents, [sideEffect])->ref,
        }
      },
    {state: initialState, sideEffects: []->ref},
  )
  React.useEffect1(() => {
    if Array.length(sideEffects.contents) > 0 {
      Array.forEach(sideEffects.contents, fn => fn({state, dispatch}))
      sideEffects := []
    }
    None
  }, [sideEffects.contents])
  (state, dispatch)
}
