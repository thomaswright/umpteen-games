open Common
open Packer

module Game = GameBase.Create({
  include Bases.FreeCell

  let forEachSpace = makeForEachSpace(
    ~freeBaseRules=Rules.FreeCell.freeBaseRules,
    ~freeRules=Rules.FreeCell.freeRules,
  )

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany([], [Card.getDeck(0, false), Card.getDeck(1, false)])->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(11),
          deckToDeal->ArrayAux.popN(11),
          deckToDeal->ArrayAux.popN(11),
          deckToDeal->ArrayAux.popN(11),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
        ],
        foundations: [[], [], [], [], [], [], [], []],
        free: [None, None, None, None, None, None, None, None],
        stock: [],
        waste: [],
      },
    )
  }

  module Board = Boards.FRT2
})
