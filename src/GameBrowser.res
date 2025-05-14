module Logo = {
  @module("./assets/logo.svg?react") @react.component
  external make: unit => React.element = "default"
}

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
  | @as("Beleaguered Castle") BeleagueredCastle
  | @as("Fortress") Fortress
  | @as("StreetsAndAlleys") StreetsAndAlleys
  | @as("Alhambra") Alhambra
  | @as("Grandfathers Clock") GrandfathersClock

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
  BeleagueredCastle,
  Fortress,
  StreetsAndAlleys,
  Alhambra,
  GrandfathersClock,
]

@react.component
let make = () => {
  let (selectGameType: gameType, setSelectGameType, _) = Common.useLocalStorage(
    "gametype",
    Klondike,
  )

  let id = selectGameType->gameString
  // bg-gradient-to-tl from-[var(--green1)] to-[var(--green2)]
  <div className={" bg-[var(--green1)] h-dvh font-sans overflow-scroll"}>
    <div>
      <div className="flex flex-row items-center  py-1 px-5 bg-[var(--green3)] ">
        <div className="h-10 w-10 mr-1">
          <Logo />
        </div>
        <div className=" font-black  text-4xl text-[var(--light)] tracking-tight">
          {"Umpteen Games"->React.string}
        </div>
        <div className="flex flex-row gap-5 text-xl ml-10">
          <select
            className={"px-2 py-1 rounded text-sm bg-[var(--light)] text-[var(--green3)]"}
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
      | BeleagueredCastle => <BeleagueredCastle.Game id />
      | Fortress => <Fortress.Game id />
      | StreetsAndAlleys => <StreetsAndAlleys.Game id />
      | Alhambra => <Alhambra.Game id />
      | GrandfathersClock => <GrandfathersClock.Game id />
      }}
    </div>
  </div>
}
