open Common
open Packer

module Game = GameBase.Create({
  include Bases.EastHaven

  let forEachSpace = Bases.EastHaven.makeForEachSpace(~stockRules=Rules.DealAll.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, true)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
        ]->GameCommons.flipLastUp,
        foundations: [[], [], [], []],
        foundations2: [],
        stock: [
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
        ],
        free: [],
        waste: [],
      },
    )
  }

  module Board = Boards.SFT
})
