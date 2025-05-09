open Common

module Game = GameBase.Create({
  include Bases.BakersGame

  let forEachSpace = Bases.BakersGame.makeForEachSpace(
    ~freeBaseRules=Rules.FreeCell.freeBaseRules,
    ~freeRules=Rules.FreeCell.freeRules,
  )

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
        foundations: [[], [], [], []],
        free: [
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
          None,
          None,
          None,
          None,
        ],
        stock: [],
        waste: [],
      },
    )
  }

  module Board = Boards.FRT3
})
