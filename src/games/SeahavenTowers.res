open Common
open Packer

module Game = GameBase.Create({
  include Bases.SeahavenTowers

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
        ],
        foundations: [[], [], [], []],
        free: [
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
          None,
          None,
        ],
        stock: [],
        waste: [],
      },
    )
  }

  let forEachSpace = Bases.SeahavenTowers.makeForEachSpace(
    ~freeBaseRules=Rules.FreeCell.freeBaseRules,
    ~freeRules=Rules.FreeCell.freeRules,
  )

  module Board = Boards.FRT
})
