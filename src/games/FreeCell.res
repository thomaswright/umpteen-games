open Common

module FreeCellBase = Packer.Make({
  let spec: Packer.spec = {
    drop: AltSuit,
    drag: AltSuit,
    size: FreeSize,
    depot: AnyDepot,
    foundation: ByOne,
  }
})

module FreeCellRules = {
  include FreeCellBase

  let initiateGame = () => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
        ],
        foundations: [[], [], [], []],
        free: [None, None, None, None],
        stock: [],
        waste: [],
      },
    )
  }

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.length == 0) &&
      game.free->Array.every(Option.isNone)
  }

  let freeBaseRules = (i): staticSpace => {
    autoProgress: DoNothing,
    droppedUpon: (game, dragPile) => {
      let noChildren = game.free->Array.getUnsafe(i)->Option.isNone

      if noChildren && dragPile->Array.length == 1 {
        Some({
          ...game,
          free: game.free->ArrayAux.update(i, _ => dragPile->Array.get(0)),
        })
      } else {
        None
      }
    },
    onClick: _ => None,
  }

  let freeRules = (card, i): movableSpace => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: 1,
      },
      baseSpace: Free(i),
      autoProgress: () => Send([card]),
      dragPile: () => Some([card]),
      droppedUpon: (_game, _dragPile) => None,
      onClick: _ => None,
      onStateChange: _ => (),
    }
  }

  let forEachSpace = FreeCellBase.makeForEachSpace(~freeBaseRules, ~freeRules)

  module StandardBoard = {
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
    ) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div className="flex flex-row gap-3">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Free(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
                className=" bg-black opacity-20   rounded w-14 h-20"
              />
            })
            ->React.array}
          </div>
          <div className="flex flex-row gap-3 ml-10">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Foundation(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
                className=" bg-white opacity-10  rounded w-14 h-20"
              />
            })
            ->React.array}
          </div>
        </div>
        <div />
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], [], [], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Pile(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
              className=" bg-black opacity-20   rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </React.Fragment>
    }
  }
}

module OneDeck = GameBase.Create({
  include FreeCellRules

  module Board = FreeCellRules.StandardBoard
})

module TwoDeck = GameBase.Create({
  include FreeCellRules

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
})

module BakersGameBase = Packer.Make({
  let spec: Packer.spec = {
    drop: OneSuit,
    drag: OneSuit,
    size: FreeSize,
    depot: AnyDepot,
    foundation: ByOne,
  }
})

module BakersGame = GameBase.Create({
  include BakersGameBase

  let initiateGame = () => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
        ],
        foundations: [[], [], [], []],
        free: [None, None, None, None],
        stock: [],
        waste: [],
      },
    )
  }

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.length == 0) &&
      game.free->Array.every(Option.isNone)
  }

  let freeBaseRules = (i): staticSpace => {
    autoProgress: DoNothing,
    droppedUpon: (game, dragPile) => {
      let noChildren = game.free->Array.getUnsafe(i)->Option.isNone

      if noChildren && dragPile->Array.length == 1 {
        Some({
          ...game,
          free: game.free->ArrayAux.update(i, _ => dragPile->Array.get(0)),
        })
      } else {
        None
      }
    },
    onClick: _ => None,
  }

  let freeRules = (card, i): movableSpace => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: 1,
      },
      baseSpace: Free(i),
      autoProgress: () => Send([card]),
      dragPile: () => Some([card]),
      droppedUpon: (_game, _dragPile) => None,
      onClick: _ => None,
      onStateChange: _ => (),
    }
  }

  let forEachSpace = BakersGameBase.makeForEachSpace(~freeBaseRules, ~freeRules)

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
    ) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div className="flex flex-row gap-3">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Free(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
                className=" bg-black opacity-20   rounded w-14 h-20"
              />
            })
            ->React.array}
          </div>
          <div className="flex flex-row gap-3 ml-10">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Foundation(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
                className=" bg-white opacity-10  rounded w-14 h-20"
              />
            })
            ->React.array}
          </div>
        </div>
        <div />
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], [], [], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Pile(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
              className=" bg-black opacity-20   rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </React.Fragment>
    }
  }
})
