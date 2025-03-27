open Webapi.Dom

type gameType = Klondike | FreeCell | UpAndDown

let gameString = (a: gameType) => (a :> string)

@decco
type state = {
  klondike: array<Klondike.Game.state>,
  freeCell: array<FreeCell.Game.state>,
  upAndDown: array<UpAndDown.Game.state>,
}

let state: ref<state> = ref({
  klondike: [],
  freeCell: [],
  upAndDown: [],
})

let setState = f => {
  let newState = f(state.contents)
  state := newState

  Dom.Storage2.localStorage->Dom.Storage2.setItem(
    "state",
    newState->state_encode->Js.Json.stringify,
  )
}

let useForceUpdate = () => {
  let (value, setValue) = React.useState(() => 0)
  () => setValue(value => value + 1)
}

@react.component
let make = () => {
  let (selectGameType: gameType, setSelectGameType) = React.useState(() => Klondike)
  let forceUpdate = useForceUpdate()

  React.useEffect0(() => {
    switch Dom.Storage2.localStorage->Dom.Storage2.getItem("state") {
    | Some(s) =>
      switch s->Js.Json.parseExn->state_decode {
      | Ok(d) => state := d
      | _ => ()
      }
    | None => setState(x => x)
    }
    forceUpdate()

    None
  })

  <div>
    <div className="px-5 pt-3 ">
      <div className="font-black text-xl mb-1"> {"Card Games!"->React.string} </div>
      <div className="flex flex-row gap-4">
        {[Klondike, FreeCell, UpAndDown]
        ->Array.map(v => {
          let selected = v == selectGameType
          <button
            key={v->gameString}
            className={[selected ? "text-blue-700 underline" : "", "font-medium"]->Array.join(" ")}
            onClick={_ => setSelectGameType(_ => v)}>
            {v->gameString->React.string}
          </button>
        })
        ->React.array}
      </div>
    </div>
    {switch selectGameType {
    | Klondike =>
      <Klondike.Game
        key={"klondike" ++ state.contents.klondike->Array.length->Int.toString}
        onCreateNewGame={x => {
          setState(state => {
            ...state,
            klondike: Array.concat([x], state.klondike),
          })
          forceUpdate()
        }}
        getState={if state.contents.klondike->Array.length == 0 {
          None
        } else {
          Some(() => state.contents.klondike->Array.getUnsafe(0))
        }}
        setState={f =>
          setState(state => {
            ...state,
            klondike: state.klondike->Common.ArrayAux.update(0, f),
          })}
      />
    | FreeCell =>
      <FreeCell.Game
        key={"freeCell" ++ state.contents.freeCell->Array.length->Int.toString}
        onCreateNewGame={x => {
          setState(state => {
            ...state,
            freeCell: Array.concat([x], state.freeCell),
          })
          forceUpdate()
        }}
        getState={if state.contents.freeCell->Array.length == 0 {
          None
        } else {
          Some(() => state.contents.freeCell->Array.getUnsafe(0))
        }}
        setState={f =>
          setState(state => {
            ...state,
            freeCell: state.freeCell->Common.ArrayAux.update(0, f),
          })}
      />
    | UpAndDown =>
      <UpAndDown.Game
        key={"upAndDown" ++ state.contents.upAndDown->Array.length->Int.toString}
        onCreateNewGame={x => {
          setState(state => {
            ...state,
            upAndDown: Array.concat([x], state.upAndDown),
          })
          forceUpdate()
        }}
        getState={if state.contents.upAndDown->Array.length == 0 {
          None
        } else {
          Some(() => state.contents.upAndDown->Array.getUnsafe(0))
        }}
        setState={f =>
          setState(state => {
            ...state,
            upAndDown: state.upAndDown->Common.ArrayAux.update(0, f),
          })}
      />
    }}
  </div>
}
