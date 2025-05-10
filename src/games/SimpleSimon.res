open Common
open Packer

module Game = GameBase.Create({
  include Bases.Spider
  let forEachSpace = makeForEachSpace(~stockRules=Rules.DealAll.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
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
        foundations2: [],
        free: [],
        waste: [],
        stock: [],
      },
    )
  }

  module Board = Boards.FT
})
