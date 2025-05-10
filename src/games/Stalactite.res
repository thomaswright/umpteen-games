open Common
open Packer

module Game = GameBase.Create({
  include Bases.Stalactite

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
        ],
        foundations: [
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
        ],
        foundations2: [],
        free: [None, None],
        stock: [],
        waste: [],
      },
    )
  }

  let forEachSpace = Bases.Stalactite.makeForEachSpace(
    ~freeBaseRules=Rules.FreeCell.freeBaseRules,
    ~freeRules=Rules.FreeCell.freeRules,
  )

  module Board = Boards.FRT
})
