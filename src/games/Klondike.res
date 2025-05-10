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
        tableau: [
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(2),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(7),
        ],
        foundations: [[], [], [], []],
        foundations2: [],
        stock: [deckToDeal.contents->Card.hideAfter(0)],
        waste: [],
        free: [],
      },
    )
  }

  let forEachSpace = Bases.Klondike.makeForEachSpace(
    ~wasteRules=Rules.WasteRotation.fannedWasteRules,
    ~stockRules=Rules.WasteRotation.stockRules,
    ~stockBaseRules=Rules.WasteRotation.stockBaseRules,
  )

  module Board = Boards.SWFT
})
