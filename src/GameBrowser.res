type gameType =
  | @as("Klondike") Klondike
  | @as("Free Cell") FreeCellOneDeck
  | @as("Free Cell: 2 Deck") FreeCellTwoDeck
  | @as("Baker's Game") BakersGame
  | @as("Up & Down") UpAndDown
  | @as("Spider: One Suit") SpiderOneSuit
  | @as("Spider: Two Suit") SpiderTwoSuit
  | @as("Spider: Four Suit") SpiderFourSuit

let gameString = (a: gameType) => (a :> string)

let allGames = [
  Klondike,
  FreeCellOneDeck,
  FreeCellTwoDeck,
  BakersGame,
  UpAndDown,
  SpiderOneSuit,
  SpiderTwoSuit,
  SpiderFourSuit,
]

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
          {allGames
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
    | Klondike => <Klondike.Game id={selectGameType->gameString} />
    | FreeCellOneDeck => <FreeCell.OneDeck id={selectGameType->gameString} />
    | FreeCellTwoDeck => <FreeCell.TwoDeck id={selectGameType->gameString} />
    | BakersGame => <FreeCell.BakersGame id={selectGameType->gameString} />
    | UpAndDown => <UpAndDown.Game id={selectGameType->gameString} />
    | SpiderOneSuit => <Spider.OneSuit id={selectGameType->gameString} />
    | SpiderTwoSuit => <Spider.TwoSuit id={selectGameType->gameString} />
    | SpiderFourSuit => <Spider.FourSuit id={selectGameType->gameString} />
    }}
  </div>
}
