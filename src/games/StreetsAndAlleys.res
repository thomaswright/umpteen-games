open Common
open Packer

module Game = GameBase.Create({
  include Bases.Diplomat

  let forEachSpace = makeForEachSpace()

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)

    let tableau = [
      deckToDeal->ArrayAux.popN(7),
      deckToDeal->ArrayAux.popN(7),
      deckToDeal->ArrayAux.popN(7),
      deckToDeal->ArrayAux.popN(7),
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(6),
    ]

    (
      shuffledDeck,
      {
        tableau,
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
