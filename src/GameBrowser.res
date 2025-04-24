type gameType =
  | @as("Klondike") Klondike
  | @as("Free Cell") FreeCell
  | @as("Free Cell: 2 Deck") DoubleFreeCell
  | @as("Up & Down") UpAndDown
  | @as("Spider: One Suit") Spider
  | @as("Spider: Two Suit") SpiderTwoSuit

let gameString = (a: gameType) => (a :> string)

@react.component
let make = () => {
  let (selectGameType: gameType, setSelectGameType, _) = Common.useLocalStorage(
    "gametype",
    Klondike,
  )

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
          {[Klondike, FreeCell, DoubleFreeCell, UpAndDown, Spider, SpiderTwoSuit]
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
    | Klondike => <Klondike.Game id={"klondike"} />
    | FreeCell => <FreeCell.Game id={"freecell"} />
    | DoubleFreeCell => <DoubleFreeCell.Game id={"freecelldouble"} />
    | UpAndDown => <UpAndDown.Game id={"upanddown"} />
    | Spider => <Spider.Game id={"spider"} />
    | SpiderTwoSuit => <SpiderTwoSuit.Game id={"spidertwosuit"} />
    }}
  </div>
}
