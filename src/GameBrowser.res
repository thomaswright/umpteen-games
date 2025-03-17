open Webapi.Dom

type gameType = Klondike_ | FreeCell_ | UpAndDown_

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
    state :=
      switch selectGameType {
      | Klondike_ => {
          ...state.contents,
          klondike: [...state.contents.klondike, Klondike.Game.createNewGame()],
        }
      | FreeCell_ => {
          ...state.contents,
          freeCell: [...state.contents.freeCell, FreeCell.Game.createNewGame()],
        }
      | UpAndDown_ => {
          ...state.contents,
          upAndDown: [...state.contents.upAndDown, UpAndDown.Game.createNewGame()],
        }
      }
  }

  let setGameType = (set, get) => {
    switch selectGameType {
    | Klondike_ => Klondike.Game.setUtils(set, get)
    | FreeCell_ => FreeCell.Game.setUtils(set, get)
    | UpAndDown_ => UpAndDown.Game.setUtils(set, get)
    }
  }

  let getGameUtils = () => {
    switch selectGameType {
    | Klondike_ =>
      Klondike.Game.setUtils(state.contents.klondike->Common.ArrayAux.getLast, f =>
        state := {
            ...state,
            klondike: state.contents.klondike->Common.ArrayAux.update(
              state.contents.klondike->Array.length - 1,
              f,
            ),
          }
      )
    | FreeCell_ => FreeCell.Game.setUtils(set, get)
    | UpAndDown_ => UpAndDown.Game.setUtils(set, get)
    }
  }

  let gameUtils = getGameUtils()

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
      <div>
        <button onClick={_ => createNewGame()}> {"New Game"->React.string} </button>
      </div>
    </div>
    {switch selectGameType {
    | Klondike_ => <Klondike.Game />
    | FreeCell_ => <FreeCell.Game />
    | UpAndDown_ => <UpAndDown.Game />
    }}
  </div>
}
