type gameType =
  | @as("Klondike") Klondike
  | @as("Free Cell") FreeCell
  | @as("Free Cell: 2 Deck") DoubleFreeCell
  | @as("Up & Down") UpAndDown
  | @as("Spider: One Suit") Spider

let gameString = (a: gameType) => (a :> string)

@decco
type state = {
  klondike: array<Klondike.Game.state>,
  freeCell: array<FreeCell.Game.state>,
  doubleFreeCell: array<DoubleFreeCell.Game.state>,
  upAndDown: array<UpAndDown.Game.state>,
  spider: array<Spider.Game.state>,
}

let state: ref<state> = ref({
  klondike: [],
  freeCell: [],
  doubleFreeCell: [],
  upAndDown: [],
  spider: [],
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
  let (_value, setValue) = React.useState(() => 0)
  () => setValue(value => value + 1)
}

@react.component
let make = () => {
  let (selectGameType: gameType, setSelectGameType, _) = Common.useLocalStorage(
    "gametype",
    Klondike,
  )
  let forceUpdate = useForceUpdate()
  let (loaded, setLoaded) = React.useState(() => false)

  React.useEffect0(() => {
    switch Dom.Storage2.localStorage->Dom.Storage2.getItem("state") {
    | Some(s) =>
      switch s->Js.Json.parseExn->state_decode {
      | Ok(d) => state := d
      | _ => ()
      }
    | None => setState(x => x)
    }
    setLoaded(_ => true)
    forceUpdate()

    None
  })
  if !loaded {
    React.null
  } else {
    <div className={" bg-gradient-to-tl from-green-900 to-green-700 h-dvh"}>
      <div className="px-5 pt-3">
        <div className="mb-1 text-white font-medium tracking-widest ">
          {"tom & won's card games"->React.string}
        </div>
        <div className="flex flex-row gap-5 text-xl">
          <select
            className={"px-2 py-1 rounded text-sm"}
            value={selectGameType->gameString}
            onChange={event => {
              setSelectGameType(_ => JsxEvent.Form.target(event)["value"])
            }}>
            {[Klondike, FreeCell, DoubleFreeCell, UpAndDown, Spider]
            ->Array.map(v => {
              <option key={v->gameString} value={v->gameString}>
                {v->gameString->React.string}
              </option>
            })
            ->React.array}
          </select>
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
      | DoubleFreeCell =>
        <DoubleFreeCell.Game
          key={"doubleFreeCell" ++ state.contents.doubleFreeCell->Array.length->Int.toString}
          onCreateNewGame={x => {
            setState(state => {
              ...state,
              doubleFreeCell: Array.concat([x], state.doubleFreeCell),
            })
            forceUpdate()
          }}
          getState={if state.contents.doubleFreeCell->Array.length == 0 {
            None
          } else {
            Some(() => state.contents.doubleFreeCell->Array.getUnsafe(0))
          }}
          setState={f =>
            setState(state => {
              ...state,
              doubleFreeCell: state.doubleFreeCell->Common.ArrayAux.update(0, f),
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
      | Spider =>
        <Spider.Game
          key={"spider" ++ state.contents.spider->Array.length->Int.toString}
          onCreateNewGame={x => {
            setState(state => {
              ...state,
              spider: Array.concat([x], state.spider),
            })
            forceUpdate()
          }}
          getState={if state.contents.spider->Array.length == 0 {
            None
          } else {
            Some(() => state.contents.spider->Array.getUnsafe(0))
          }}
          setState={f =>
            setState(state => {
              ...state,
              spider: state.spider->Common.ArrayAux.update(0, f),
            })}
        />
      }}
    </div>
  }
}
