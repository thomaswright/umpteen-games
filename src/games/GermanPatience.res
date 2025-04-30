open Common

module GermanPatienceBase = Packer.Make({
  let spec: Packer.spec = {
    drop: CyclicAnySuit,
    drag: CyclicAnySuit,
    size: AnySize,
    depot: AnyDepot,
    foundation: NoFoundation,
  }
})

module GermanPatienceRules = {
  include GermanPatienceBase

  let winCheck = (game: Packer.game) => {
    game.piles->Array.every(pile =>
      pile->Array.length == 13 && pile->GameCommons.decCyclicAnySuitValidation
    )
  }

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany([], [Card.getDeck(0, true), Card.getDeck(1, true)])->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
        ]->GameCommons.flipLastUp,
        foundations: [],
        stock: [
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
        ],
        waste: [],
        free: [],
      },
    )
  }

  let stockRules = (_game, _card, i, j): movableSpace => {
    {
      locationAdjustment: {
        x: i * 20,
        y: 0,
        z: i * 10 + j + 1,
      },
      baseSpace: Stock,
      dragPile: () => {
        None
      },
      autoProgress: () => DoNothing,
      droppedUpon: (_game, _dragPile) => {
        None
      },
      onClick: game => {
        game.stock
        ->Common.ArrayAux.getLast
        ->Option.map(stockGroup => {
          {
            ...game,
            piles: game.piles
            ->Array.mapWithIndex((pile, i) => {
              stockGroup->Array.get(i)->Option.mapOr(pile, v => Array.concat(pile, [v]))
            })
            ->GameCommons.flipLastUp,
            stock: game.stock->Array.slice(~start=0, ~end=game.stock->Array.length - 1),
          }
        })
      },
      onStateChange: element => {
        Card.hide(element)
      },
    }
  }

  let forEachSpace = GermanPatienceBase.makeForEachSpace(~stockRules)

  module Board = {
    @react.component
    let make = (~setRef, ~initialGame: Packer.game) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div
            key={Stock->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Packer.Stock))}
            className=" bg-white opacity-10  rounded w-14 h-20 mr-20"
          />
        </div>
        <div />
        <div className="flex flex-row gap-3 mt-5">
          {Array.make(~length=initialGame.piles->Array.length, [])
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

module Game = GameBase.Create(GermanPatienceRules)
