open Common

let flipLastUp = (piles: array<array<Card.sides>>) =>
  piles->Array.map(pile => pile->ArrayAux.updateLast(v => {...v, hidden: false}))

module SpiderBase = Packer.Make({
  let spec: Packer.spec = {
    drop: AnySuit,
    drag: OneSuit,
    size: AnySize,
    depot: AnyDepot,
    foundation: ByAll,
  }
})

module SpiderRules = {
  include SpiderBase

  let winCheck = (game: Packer.game) => {
    game.piles->Array.every(pile => pile->Array.length == 0) &&
      game.stock->Array.every(stockGroup => stockGroup->Array.length == 0)
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
            ->flipLastUp,
            stock: game.stock->Array.slice(~start=0, ~end=game.stock->Array.length - 1),
          }
        })
      },
      onStateChange: element => {
        Card.hide(element)
      },
    }
  }

  let forEachSpace = SpiderBase.makeForEachSpace(~stockRules)

  module StandardBoard = {
    @react.component
    let make = (~setRef, ~initialGame: Packer.game) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div
            key={Stock->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Packer.Stock))}
            className=" bg-white opacity-10  rounded w-14 h-20 mr-20"
          />
          <div className="flex flex-row gap-3 ml-10">
            {Array.make(~length=initialGame.foundations->Array.length, [])
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

module OneSuit = GameBase.Create({
  include SpiderRules

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany(
        [],
        [
          Card.getOneSuitDeck(0, Spades, true),
          Card.getOneSuitDeck(1, Spades, true),
          Card.getOneSuitDeck(2, Spades, true),
          Card.getOneSuitDeck(3, Spades, true),
          Card.getOneSuitDeck(4, Spades, true),
          Card.getOneSuitDeck(5, Spades, true),
          Card.getOneSuitDeck(6, Spades, true),
          Card.getOneSuitDeck(7, Spades, true),
        ],
      )->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
        ]->flipLastUp,
        foundations: [[], [], [], [], [], [], [], []],
        stock: [
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
        ],
        waste: [],
        free: [],
      },
    )
  }

  module Board = SpiderRules.StandardBoard
})

module TwoSuit = GameBase.Create({
  include SpiderRules

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany(
        [],
        [
          Card.getOneSuitDeck(0, Spades, true),
          Card.getOneSuitDeck(1, Spades, true),
          Card.getOneSuitDeck(2, Spades, true),
          Card.getOneSuitDeck(3, Spades, true),
          Card.getOneSuitDeck(4, Hearts, true),
          Card.getOneSuitDeck(5, Hearts, true),
          Card.getOneSuitDeck(6, Hearts, true),
          Card.getOneSuitDeck(7, Hearts, true),
        ],
      )->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
        ]->flipLastUp,
        foundations: [[], [], [], [], [], [], [], []],
        stock: [
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
        ],
        waste: [],
        free: [],
      },
    )
  }
  module Board = SpiderRules.StandardBoard
})

module FourSuit = GameBase.Create({
  include SpiderRules

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany([], [Card.getDeck(0, true), Card.getDeck(1, true)])->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
        ]->flipLastUp,
        foundations: [[], [], [], [], [], [], [], []],
        stock: [
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
        ],
        waste: [],
        free: [],
      },
    )
  }
  module Board = SpiderRules.StandardBoard
})

module SimpleSimonRules = {
  include SpiderRules

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(2),
          deckToDeal->ArrayAux.popN(1),
        ],
        foundations: [[], [], [], []],
        free: [],
        waste: [],
        stock: [],
      },
    )
  }

  module Board = {
    @react.component
    let make = (~setRef, ~initialGame: Packer.game) => {
      <React.Fragment>
        <div className="flex flex-row gap-3 mt-5">
          {Array.make(~length=initialGame.foundations->Array.length, [])
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Foundation(i)->spaceToString}
              id={Foundation(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Packer.Foundation(i)))}
              className=" bg-white opacity-10 rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
        <div className="flex flex-row gap-3 mt-5">
          {Array.make(~length=initialGame.piles->Array.length, [])
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Pile(i)->spaceToString}
              id={Pile(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
              className=" bg-black opacity-20  rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </React.Fragment>
    }
  }
}
module SimpleSimon = GameBase.Create(SimpleSimonRules)

module MsMop = GameBase.Create({
  include SpiderRules

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany([], [Card.getDeck(0, false), Card.getDeck(1, false)])->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
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
          deckToDeal->ArrayAux.popN(8),
        ]->flipLastUp,
        foundations: [[], [], [], [], [], [], [], []],
        stock: [],
        waste: [],
        free: [],
      },
    )
  }
  module Board = SimpleSimonRules.Board
})

module ScorpionBase = Packer.Make({
  let spec: Packer.spec = {
    drop: OneSuit,
    drag: AnySuit,
    size: AnySize,
    depot: SpecificDepot(RK),
    foundation: ByAll,
  }
})

module ScorpionRules = {
  include ScorpionBase

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, true)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(3),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(3),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(3),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(0),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(0),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(0),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(0),
        ]->flipLastUp,
        foundations: [[], [], [], []],
        stock: [deckToDeal->ArrayAux.popN(3)],
        waste: [],
        free: [],
      },
    )
  }

  let winCheck = SpiderRules.winCheck

  let stockRules = SpiderRules.stockRules

  let forEachSpace = ScorpionBase.makeForEachSpace(~stockRules)

  module Board = SpiderRules.StandardBoard
}

module Scorpion = GameBase.Create(ScorpionRules)
