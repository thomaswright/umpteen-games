open Common

module Game = GameBase.Create({
  include Bases.Diplomat

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany([], [Card.getDeck(0, false), Card.getDeck(1, false)])->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(4),
        ],
        foundations: [[], [], [], [], [], [], [], []],
        free: [],
        stock: [deckToDeal.contents->Card.hideAfter(0)],
        waste: [],
      },
    )
  }

  let forEachSpace = Bases.Diplomat.makeForEachSpace(
    ~wasteRules=Rules.WasteRotation.wasteRules,
    ~stockRules=Rules.WasteRotation.stockRules,
    ~stockBaseRules=Rules.WasteRotation.stockBaseRules,
  )
  module Board = Boards.SWFT
})
