open Common
open Packer

module OneSuit = GameBase.Create({
  include Bases.Spider

  let forEachSpace = makeForEachSpace(~stockRules=Rules.DealAll.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany(
        [],
        [
          Card.getOneSuitDeck(0, Spades, true),
          Card.getOneSuitDeck(1, Spades, true),
          Card.getOneSuitDeck(2, Spades, true),
          Card.getOneSuitDeck(3, Spades, true),
          Card.getOneSuitDeck(4, Spades, true),
          Card.getOneSuitDeck(5, Spades, true),
          Card.getOneSuitDeck(6, Spades, true),
          Card.getOneSuitDeck(7, Spades, true),
        ],
      )->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
        ]->GameCommons.flipLastUp,
        foundations: [[], [], [], [], [], [], [], []],
        foundations2: [],
        stock: [
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
        ],
        waste: [],
        free: [],
      },
    )
  }

  module Board = Boards.SFT
})

module TwoSuit = GameBase.Create({
  include Bases.Spider
  let forEachSpace = makeForEachSpace(~stockRules=Rules.DealAll.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany(
        [],
        [
          Card.getOneSuitDeck(0, Spades, true),
          Card.getOneSuitDeck(1, Spades, true),
          Card.getOneSuitDeck(2, Spades, true),
          Card.getOneSuitDeck(3, Spades, true),
          Card.getOneSuitDeck(4, Hearts, true),
          Card.getOneSuitDeck(5, Hearts, true),
          Card.getOneSuitDeck(6, Hearts, true),
          Card.getOneSuitDeck(7, Hearts, true),
        ],
      )->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
        ]->GameCommons.flipLastUp,
        foundations: [[], [], [], [], [], [], [], []],
        foundations2: [],
        stock: [
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
        ],
        waste: [],
        free: [],
      },
    )
  }
  module Board = Boards.SFT
})

module FourSuit = GameBase.Create({
  include Bases.Spider
  let forEachSpace = makeForEachSpace(~stockRules=Rules.DealAll.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany([], [Card.getDeck(0, true), Card.getDeck(1, true)])->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
        ]->GameCommons.flipLastUp,
        foundations: [[], [], [], [], [], [], [], []],
        foundations2: [],
        stock: [
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
          deckToDeal->ArrayAux.popN(10),
        ],
        waste: [],
        free: [],
      },
    )
  }
  module Board = Boards.SFT
})
