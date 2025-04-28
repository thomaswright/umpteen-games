open Webapi.Dom
open Common

module KlondikeBase = Packer.Make({
  let spec: Packer.spec = {
    drop: AltSuit,
    drag: AltSuit,
    size: AnySize,
    depot: KingDepot,
    foundation: ByOne,
  }
})

module GameRules: GameBase.GameRules = {
  include KlondikeBase

  let initiateGame = () => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(2),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(7),
        ],
        foundations: [[], [], [], []],
        stock: [deckToDeal.contents],
        waste: [],
        free: [],
      },
    )
  }

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.length == 0) &&
    game.stock->Array.length == 0 &&
    game.waste->Array.length == 0
  }

  let wasteRules = (game, card, i): movableSpace => {
    baseSpace: Waste,
    locationAdjustment: {
      x: 20 * i,
      y: 0,
      z: i + 1,
    },
    dragPile: () => {
      if i == game.waste->Array.length - 1 {
        Some([card])
      } else {
        None
      }
    },
    autoProgress: () => DoNothing,
    droppedUpon: (_, _) => None,
    onClick: _ => None,
    onStateChange: element => Card.showOrHide(card, element),
  }

  let stockRules = (card, i): movableSpace => {
    baseSpace: Stock,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: i + 1,
    },
    dragPile: () => None,
    autoProgress: () => DoNothing,
    droppedUpon: (_, _) => None,
    onClick: game => {
      let realStock = game.stock->Array.getUnsafe(0)
      Some({
        ...game,
        stock: game.stock->ArrayAux.update(0, v =>
          v->Array.slice(~start=0, ~end=realStock->Array.length - 1)
        ),
        waste: game.waste->Array.concat(
          realStock->Array.sliceToEnd(~start=realStock->Array.length - 1),
        ),
      })
    },
    onStateChange: element => Card.showOrHide(card, element),
  }

  let stockBaseRules = (): staticSpace => {
    autoProgress: DoNothing,
    droppedUpon: (_game, _dragPile) => {
      None
    },
    onClick: game => {
      Some({
        ...game,
        stock: [game.waste->Array.toReversed],
        waste: [],
      })
    },
  }

  let forEachSpace = (game: game, f) => {
    game.piles->Array.forEachWithIndex((pile, i) => {
      f(Pile(i), pileBaseRules(game, i)->GameBase.Static)

      pile->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), pileRules(pile, card, i, j)->Movable)
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      f(Foundation(i), foundationBaseRules(i)->Static)

      foundation->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), foundationRules(game, card, i, j)->Movable)
      })
    })

    game.waste->Array.forEachWithIndex((card, i) => {
      f(Card(card.card), wasteRules(game, card, i)->Movable)
    })

    game.stock
    ->Array.getUnsafe(0)
    ->Array.forEachWithIndex((card, i) => {
      f(Card(card.card), stockRules(card, i)->Movable)
    })

    f(Stock, stockBaseRules()->Static)
  }

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
        <div className="flex flex-row gap-3">
          <div
            key={Stock->spaceToString}
            id={Stock->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Stock))}
            className=" bg-black opacity-20 rounded w-14 h-20"
          />
          <div
            key={Waste->spaceToString}
            id={Waste->spaceToString}
            ref={ReactDOM.Ref.callbackDomRef(setRef(Waste))}
            className=" w-14 h-20"
          />
        </div>
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Foundation(i)->spaceToString}
              id={Foundation(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
              className=" bg-white opacity-10 rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], [], [], [], []]
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

module Game = GameBase.Create(GameRules)
