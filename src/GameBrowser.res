type gameType =
  | @as("Klondike") Klondike
  | @as("Free Cell") FreeCellOneDeck
  | @as("Free Cell: 2 Deck") FreeCellTwoDeck
  | @as("Baker's Game") BakersGame
  | @as("Eight Off") EightOff
  | @as("Seahaven Towers") SeahavenTowers
  | @as("Up & Down") UpAndDown
  | @as("Spider: One Suit") SpiderOneSuit
  | @as("Spider: Two Suit") SpiderTwoSuit
  | @as("Spider: Four Suit") SpiderFourSuit
  | @as("Scorpion") Scorpion
  | @as("Pyramid") Pyramid
  | @as("Simple Simon") SimpleSimon
  | @as("East Haven") EastHaven

let gameString = (a: gameType) => (a :> string)

let allGames = [
  Klondike,
  FreeCellOneDeck,
  FreeCellTwoDeck,
  BakersGame,
  EightOff,
  SeahavenTowers,
  UpAndDown,
  SpiderOneSuit,
  SpiderTwoSuit,
  SpiderFourSuit,
  Scorpion,
  SimpleSimon,
  Pyramid,
  EastHaven,
]

@react.component
let make = () => {
  let (selectGameType: gameType, setSelectGameType, _) = Common.useLocalStorage(
    "gametype",
    Klondike,
  )

  let id = selectGameType->gameString

  <div className={" bg-gradient-to-tl from-green-900 to-green-700 h-dvh"}>
    <div className="px-5 pt-3">
      <div className="mb-1 text-white font-medium tracking-widest ">
        {"tom & won's card games"->React.string}
      </div>
      <div className="flex flex-row gap-5 text-xl">
        <select
          className={"px-2 py-1 rounded text-sm"}
          value={id}
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
    | Klondike => <Klondike.Game id />
    | FreeCellOneDeck => <FreeCell.OneDeck id />
    | FreeCellTwoDeck => <FreeCell.TwoDeck id />
    | BakersGame => <FreeCell.BakersGame id />
    | EightOff => <FreeCell.EightOff id />
    | SeahavenTowers => <FreeCell.SeahavenTowers id />
    | UpAndDown => <UpAndDown.Game id />
    | SpiderOneSuit => <Spider.OneSuit id />
    | SpiderTwoSuit => <Spider.TwoSuit id />
    | SpiderFourSuit => <Spider.FourSuit id />
    | Scorpion => <Spider.Scorpion id />
    | SimpleSimon => <Spider.SimpleSimon id />
    | Pyramid => <Pyramid.Game id />
    | EastHaven => <EastHaven.Game id />
    }}
  </div>
}
