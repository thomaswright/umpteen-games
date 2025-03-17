open Webapi.Dom

type gameType = Klondike_ | FreeCell_ | UpAndDown_

let gameString = (a: gameType) => (a :> string)

@decco
type gameData =
  Klondike(Klondike.Game.state) | FreeCell(FreeCell.Game.state) | UpAndDown(UpAndDown.Game.state)

@decco
type state = array<gameData>

let state: ref<state> = ref([])

let setState = f => {
  let newState = f(state.contents)
  state := newState
  Dom.Storage2.localStorage->Dom.Storage2.setItem(
    "state",
    newState->state_encode->Js.Json.stringify,
  )
}

@react.component
let make = () => {
  let (selectGameType: gameType, setSelectGameType) = React.useState(() => Klondike_)

  React.useEffect0(() => {
    Dom.Storage2.localStorage
    ->Dom.Storage2.getItem("state")
    ->Option.mapOr((), s => {
      switch s->Js.Json.parseExn->state_decode {
      | Ok(d) => state := d
      | _ => ()
      }
    })
    None
  })

  let createNewGame = () => {
    state := [...state.contents]
  }
  <div>
    <div className="px-5 pt-3 ">
      <div className="font-black text-xl mb-1"> {"Card Games!"->React.string} </div>
      <div className="flex flex-row gap-4">
        {[Klondike_, FreeCell_, UpAndDown_]
        ->Array.map(v => {
          let selected = v == selectGameType
          <button
            className={[selected ? "text-blue-700 underline" : "", "font-medium"]->Array.join(" ")}
            onClick={_ => setSelectGameType(_ => v)}>
            {v->gameString->React.string}
          </button>
        })
        ->React.array}
      </div>
    </div>
    {switch selectGameType {
    | Klondike_ => <Klondike.Game />
    | FreeCell_ => <FreeCell.Game />
    | UpAndDown_ => <UpAndDown.Game />
    }}
  </div>
}
