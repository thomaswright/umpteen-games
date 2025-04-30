open Common
open Packer

module Game = GameBase.Create({
  include Bases.EastHaven

  let forEachSpace = Bases.EastHaven.makeForEachSpace(~stockRules=Spider.SpiderRules.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, true)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(3),
        ]->GameCommons.flipLastUp,
        foundations: [[], [], [], []],
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

  module Board = Boards.Spider
})
