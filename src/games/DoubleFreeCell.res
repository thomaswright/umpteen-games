module GameRules: GameBase.GameRules = {
  include FreeCell.FreeCellRules

  module Board = {
    @react.component
    let make = (
      ~setRef,
      ~onMouseDown as _,
      ~setGame as _,
      ~moveToState as _,
      ~autoProgress as _,
      ~game as _,
      ~undo as _,
      ~isWin as _,
      ~onClick as _,
    ) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div className="flex flex-col gap-3">
            <div className="flex flex-row gap-3">
              {[[], [], [], []]
              ->Array.mapWithIndex((_, i) => {
                <div
                  key={Free(i)->spaceToString}
                  ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
                  className="   bg-black opacity-20  rounded w-14 h-20"
                />
              })
              ->React.array}
            </div>
            <div className="flex flex-row gap-3">
              {[[], [], [], []]
              ->Array.mapWithIndex((_, i) => {
                <div
                  key={Free(i + 4)->spaceToString}
                  ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i + 4)))}
                  className="   bg-black opacity-20  rounded w-14 h-20"
                />
              })
              ->React.array}
            </div>
          </div>
          <div className="flex flex-col gap-3">
            <div className="flex flex-row gap-3 ml-10">
              {[[], [], [], []]
              ->Array.mapWithIndex((_, i) => {
                <div
                  key={Free(i)->spaceToString}
                  ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
                  className="   bg-white opacity-10  rounded w-14 h-20"
                />
              })
              ->React.array}
            </div>
            <div className="flex flex-row gap-3 ml-10">
              {[[], [], [], []]
              ->Array.mapWithIndex((_, i) => {
                <div
                  key={Free(i + 4)->spaceToString}
                  ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i + 4)))}
                  className="   bg-white opacity-10  rounded w-14 h-20"
                />
              })
              ->React.array}
            </div>
          </div>
        </div>
        <div />
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], [], [], [], [], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Pile(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
              className=" bg-black opacity-20  rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </React.Fragment>
    }
  }

  module AllCards = {
    @react.component
    let make = (~setRef, ~onMouseDown, ~onClick, ~deck) => {
      <React.Fragment>
        {deck
        ->Array.map(card => {
          <Card.Display
            card={card}
            key={Card(card.card)->spaceToString}
            id={Card(card.card)->spaceToString}
            cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card.card)))}
            onMouseDown={onMouseDown}
            onClick
          />
        })
        ->React.array}
      </React.Fragment>
    }
  }
}

module Game = GameBase.Create(GameRules)
