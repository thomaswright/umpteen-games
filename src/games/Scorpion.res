open Common
open Packer

module Game = GameBase.Create({
  include Bases.Scorpion

  let forEachSpace = makeForEachSpace(~stockRules=Rules.DealAll.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, true)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(3),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(3),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(3),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(0),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(0),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(0),
          deckToDeal->ArrayAux.popN(7)->Card.showAfter(0),
        ]->GameCommons.flipLastUp,
        foundations: [[], [], [], []],
        stock: [deckToDeal->ArrayAux.popN(3)],
        waste: [],
        free: [],
      },
    )
  }

  module Board = Boards.SFT
})
