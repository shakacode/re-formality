type update('action, 'state) =
  | NoUpdate
  | Update('state)
  | UpdateWithSideEffects('state, self('state, 'action) => unit)
and dispatch('action) = 'action => unit
and self('state, 'action) = {
  state: 'state,
  dispatch: dispatch('action),
}
and fullState('state, 'action) = {
  state: 'state,
  sideEffects: ref(array(self('state, 'action) => unit)),
};

let useReducer = (initialState, reducer) => {
  let ({state, sideEffects}, dispatch) =
    React.useReducer(
      ({state, sideEffects} as fullState, action) =>
        switch (reducer(state, action)) {
        | NoUpdate => fullState
        | Update(state) => {...fullState, state}
        | UpdateWithSideEffects(state, sideEffect) => {
            state,
            sideEffects: Array.concat(sideEffects^, [|sideEffect|])->ref,
          }
        },
      {state: initialState, sideEffects: [||]->ref},
    );
  React.useEffect1(
    () => {
      if (Array.length(sideEffects^) > 0) {
        Array.forEach(sideEffects^, fn => fn({state, dispatch}));
        sideEffects := [||];
      };
      None;
    },
    [|sideEffects^|],
  );
  (state, dispatch);
};
