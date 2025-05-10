type gameType =
  | @as("Klondike") Klondike
  | @as("Free Cell") FreeCellOneDeck
  | @as("Free Cell: 2 Deck") FreeCellTwoDeck
  | @as("Baker's Game") BakersGame
  | @as("Eight Off") EightOff
  | @as("Seahaven Towers") SeahavenTowers
  | @as("Penguin") Penguin
  | @as("Up & Down") UpAndDown
  | @as("Spider: One Suit") SpiderOneSuit
  | @as("Spider: Two Suit") SpiderTwoSuit
  | @as("Spider: Four Suit") SpiderFourSuit
  | @as("Scorpion") Scorpion
  | @as("Pyramid") Pyramid
  | @as("Simple Simon") SimpleSimon
  | @as("Mrs Mop") MrsMop
  | @as("East Haven") EastHaven
  | @as("Stalactite") Stalactite
  | @as("German Patience") GermanPatience
  | @as("Gay Gordon's") GayGordons
  | @as("Agnes Sorel") AgnesSorel
  | @as("Agnes Bernauer") AgnesBernauer
  | @as("Diplomat") Diplomat
  | @as("Alhambra") Alhambra

let gameString = (a: gameType) => (a :> string)

let allGames = [
  Klondike,
  FreeCellOneDeck,
  FreeCellTwoDeck,
  BakersGame,
  EightOff,
  SeahavenTowers,
  Penguin,
  UpAndDown,
  SpiderOneSuit,
  SpiderTwoSuit,
  SpiderFourSuit,
  Scorpion,
  SimpleSimon,
  MrsMop,
  Pyramid,
  EastHaven,
  Stalactite,
  GermanPatience,
  GayGordons,
  AgnesSorel,
  AgnesBernauer,
  Diplomat,
  Alhambra,
]

@react.component
let make = () => {
  let (selectGameType: gameType, setSelectGameType, _) = Common.useLocalStorage(
    "gametype",
    Klondike,
  )

  let id = selectGameType->gameString

  <div className={" bg-gradient-to-tl from-[#003c18] to-[#006f2a] h-dvh"}>
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
    | FreeCellOneDeck => <FreeCellOneDeck.Game id />
    | FreeCellTwoDeck => <FreeCellTwoDeck.Game id />
    | BakersGame => <BakersGame.Game id />
    | EightOff => <EightOff.Game id />
    | SeahavenTowers => <SeahavenTowers.Game id />
    | Penguin => <Penguin.Game id />
    | UpAndDown => <UpAndDown.Game id />
    | SpiderOneSuit => <Spider.OneSuit id />
    | SpiderTwoSuit => <Spider.TwoSuit id />
    | SpiderFourSuit => <Spider.FourSuit id />
    | Scorpion => <Scorpion.Game id />
    | SimpleSimon => <SimpleSimon.Game id />
    | MrsMop => <MrsMop.Game id />
    | Pyramid => <Pyramid.Game id />
    | EastHaven => <EastHaven.Game id />
    | Stalactite => <Stalactite.Game id />
    | GermanPatience => <GermanPatience.Game id />
    | GayGordons => <GayGordons.Game id />
    | AgnesSorel => <AgnesSorel.Game id />
    | AgnesBernauer => <AgnesBernauer.Game id />
    | Diplomat => <Diplomat.Game id />
    | Alhambra => <Alhambra.Game id />
    }}
  </div>
}
