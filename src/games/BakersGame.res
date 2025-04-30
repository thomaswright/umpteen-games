open Common

module BakersGame = GameBase.Create({
  include Bases.BakersGame

  let winCheck = FreeCell.FreeCellRules.winCheck

  let forEachSpace = Bases.BakersGame.makeForEachSpace(
    ~freeBaseRules=FreeCell.FreeCellRules.freeBaseRules,
    ~freeRules=FreeCell.FreeCellRules.freeRules,
  )

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
        ],
        foundations: [[], [], [], []],
        free: [None, None, None, None],
        stock: [],
        waste: [],
      },
    )
  }
  module Board = Boards.FreeCell
})

module EightOff = GameBase.Create({
  include Bases.BakersGame

  let winCheck = FreeCell.FreeCellRules.winCheck

  let forEachSpace = Bases.BakersGame.makeForEachSpace(
    ~freeBaseRules=FreeCell.FreeCellRules.freeBaseRules,
    ~freeRules=FreeCell.FreeCellRules.freeRules,
  )

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
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

  module Board = Boards.EightOff
})
