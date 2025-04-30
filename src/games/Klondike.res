open Common
open Packer

module Game = GameBase.Create({
  include Bases.Klondike

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
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
        stock: [deckToDeal.contents->Card.hideAfter(0)],
        waste: [],
        free: [],
      },
    )
  }

  let wasteRules = (game: Packer.game, card, i): movableSpace => {
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

  let stockRules = (_game, card, _i, j): movableSpace => {
    baseSpace: Stock,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: j + 1,
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
          realStock
          ->Array.sliceToEnd(~start=realStock->Array.length - 1)
          ->Card.showAfter(0),
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
        stock: [game.waste->Array.toReversed->Card.hideAfter(0)],
        waste: [],
      })
    },
  }

  let forEachSpace = Bases.Klondike.makeForEachSpace(~wasteRules, ~stockRules, ~stockBaseRules)

  module Board = Boards.Klondike
})
