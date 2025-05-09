open Common
open Packer

module SpiderRules = {
  let stockRules = (_game, _card, i, j): movableSpace => {
    {
      locationAdjustment: {
        x: i * 20,
        y: 0,
        z: i * 10 + j + 1,
      },
      baseSpace: Stock,
      dragPile: () => None,
      autoProgress: () => DoNothing,
      droppedUpon: (_game, _dragPile) => None,
      onClick: game => {
        game.stock
        ->Common.ArrayAux.getLast
        ->Option.map(stockGroup => {
          {
            ...game,
            piles: game.piles
            ->Array.mapWithIndex((pile, i) => {
              stockGroup->Array.get(i)->Option.mapOr(pile, v => Array.concat(pile, [v]))
            })
            ->GameCommons.flipLastUp,
            stock: game.stock->Array.slice(~start=0, ~end=game.stock->Array.length - 1),
          }
        })
      },
      onStateChange: element => {
        Card.hide(element)
      },
    }
  }
}

module OneSuit = GameBase.Create({
  include Bases.Spider

  let forEachSpace = makeForEachSpace(~stockRules=SpiderRules.stockRules)

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
        piles: [
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

  module Board = Boards.Spider
})

module TwoSuit = GameBase.Create({
  include Bases.Spider
  let forEachSpace = makeForEachSpace(~stockRules=SpiderRules.stockRules)

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
        piles: [
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
  module Board = Boards.Spider
})

module FourSuit = GameBase.Create({
  include Bases.Spider
  let forEachSpace = makeForEachSpace(~stockRules=SpiderRules.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany([], [Card.getDeck(0, true), Card.getDeck(1, true)])->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
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
  module Board = Boards.Spider
})

module SimpleSimon = GameBase.Create({
  include Bases.Spider
  let forEachSpace = makeForEachSpace(~stockRules=SpiderRules.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(2),
          deckToDeal->ArrayAux.popN(1),
        ],
        foundations: [[], [], [], []],
        free: [],
        waste: [],
        stock: [],
      },
    )
  }

  module Board = Boards.SimpleSimon
})

module MrsMop = GameBase.Create({
  include Bases.Spider
  let forEachSpace = makeForEachSpace(~stockRules=SpiderRules.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck =
      Array.concatMany([], [Card.getDeck(0, false), Card.getDeck(1, false)])->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
          deckToDeal->ArrayAux.popN(8),
        ]->GameCommons.flipLastUp,
        foundations: [[], [], [], [], [], [], [], []],
        stock: [],
        waste: [],
        free: [],
      },
    )
  }
  module Board = Boards.SimpleSimon
})

module Scorpion = GameBase.Create({
  include Bases.Scorpion

  let forEachSpace = makeForEachSpace(~stockRules=SpiderRules.stockRules)

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, true)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
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

  module Board = Boards.Spider
})
