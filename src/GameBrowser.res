type gameType = Klondike | FreeCell | UpAndDown

let gameString = (a: gameType) => (a :> string)

@react.component
let make = () => {
  let (selectGame, setSelectGame) = React.useState(() => Klondike)
  <div>
    <div className="px-5 pt-3 ">
      <div className="font-black text-xl mb-1"> {"Card Games!"->React.string} </div>
      <div className="flex flex-row gap-4">
        {[Klondike, FreeCell, UpAndDown]
        ->Array.map(v => {
          let selected = v == selectGame
          <button
            className={[selected ? "text-blue-700 underline" : "", "font-medium"]->Array.join(" ")}
            onClick={_ => setSelectGame(_ => v)}>
            {v->gameString->React.string}
          </button>
        })
        ->React.array}
      </div>
    </div>
    {switch selectGame {
    | Klondike => <Klondike.Game />
    | FreeCell => <FreeCell.Game />
    | UpAndDown => <UpAndDown.Game />
    }}
  </div>
}
