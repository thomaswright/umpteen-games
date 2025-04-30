open Common
open Packer

module Game = GameBase.Create({
  include Bases.GermanPatience

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

  let forEachSpace = Bases.GermanPatience.makeForEachSpace(~stockRules)

  module Board = Boards.GermanPatience
})
