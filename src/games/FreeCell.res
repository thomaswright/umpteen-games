open Common
open Packer

module FreeCellRules = {
  let freeBaseRules = (i): staticSpace => {
    autoProgress: DoNothing,
    droppedUpon: (game, dragPile) => {
      let noChildren = game.free->Array.getUnsafe(i)->Option.isNone

      if noChildren && dragPile->Array.length == 1 {
        Some({
          ...game,
          free: game.free->ArrayAux.update(i, _ => dragPile->Array.get(0)),
        })
      } else {
        None
      }
    },
    onClick: _ => None,
  }

  let freeRules = (card, i): movableSpace => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: 1,
      },
      baseSpace: Free(i),
      autoProgress: () => Send([card]),
      dragPile: () => Some([card]),
      droppedUpon: (_game, _dragPile) => None,
      onClick: _ => None,
      onStateChange: element => Card.showOrHide(card, element),
    }
  }
}

module OneDeck = GameBase.Create({
  include Bases.FreeCell

  let forEachSpace = makeForEachSpace(
    ~freeBaseRules=FreeCellRules.freeBaseRules,
    ~freeRules=FreeCellRules.freeRules,
  )

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
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
  module Board = Boards.FRT
})

module TwoDeck = GameBase.Create({
  include Bases.FreeCell

  let forEachSpace = makeForEachSpace(
    ~freeBaseRules=FreeCellRules.freeBaseRules,
    ~freeRules=FreeCellRules.freeRules,
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

module SeahavenTowers = GameBase.Create({
  include Bases.SeahavenTowers

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(5),
        ],
        foundations: [[], [], [], []],
        free: [
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
          None,
          None,
        ],
        stock: [],
        waste: [],
      },
    )
  }

  let forEachSpace = Bases.SeahavenTowers.makeForEachSpace(
    ~freeBaseRules=FreeCellRules.freeBaseRules,
    ~freeRules=FreeCellRules.freeRules,
  )

  module Board = Boards.FRT
})

module Penguin = GameBase.Create({
  include Bases.Penguin

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    let beak = deckToDeal.contents->Array.getUnsafe(0)

    let otherBeaks = []

    deckToDeal :=
      deckToDeal.contents->Array.filterMap(v => {
        if v.card.rank == beak.card.rank && v != beak {
          otherBeaks->Array.push(v)
          None
        } else {
          Some(v)
        }
      })

    (
      shuffledDeck,
      {
        tableau: [
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
        ],
        foundations: [
          [],
          [otherBeaks->Array.getUnsafe(0)],
          [otherBeaks->Array.getUnsafe(1)],
          [otherBeaks->Array.getUnsafe(2)],
        ],
        free: [None, None, None, None, None, None, None],
        stock: [],
        waste: [],
      },
    )
  }

  let foundationBaseRules = (i): staticSpace => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        let justOne = dragPile->Array.length == 1

        let dragPileBase = dragPile->Array.getUnsafe(0)
        let noChildren = game.tableau->Array.getUnsafe(i)->Array.length == 0
        let second = game.foundations->Array.getUnsafe(1)->Array.getUnsafe(0)

        if noChildren && justOne && dragPileBase.card.rank == second.card.rank {
          Some({
            ...game,
            tableau: game.tableau->GameCommons.flipLastUp,
            foundations: game.foundations->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
      onClick: _ => None,
    }
  }

  let tableauBaseRules = (game: Packer.game, i): staticSpace => {
    {
      droppedUpon: (gameRemoved, dragPile) => {
        let dragPileBase = dragPile->Array.getUnsafe(0)
        let noChildren = game.tableau->Array.getUnsafe(i)->Array.length == 0
        let second = game.foundations->Array.getUnsafe(1)->Array.getUnsafe(0)

        if noChildren && Card.rankIsAboveCyclic(second, dragPileBase) {
          Some({
            ...gameRemoved,
            tableau: gameRemoved.tableau->ArrayAux.update(i, _ => dragPile)->GameCommons.flipLastUp,
          })
        } else {
          None
        }
      },
      autoProgress: Accept,
      onClick: _ => None,
    }
  }

  let forEachSpace = Bases.Penguin.makeForEachSpace(
    ~tableauBaseRules,
    ~freeBaseRules=FreeCellRules.freeBaseRules,
    ~freeRules=FreeCellRules.freeRules,
    ~foundationBaseRules,
  )

  module Board = Boards.FRT3
})

module Stalactite = GameBase.Create({
  include Bases.Stalactite

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
        foundations: [
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(1),
        ],
        free: [None, None],
        stock: [],
        waste: [],
      },
    )
  }

  let forEachSpace = Bases.Stalactite.makeForEachSpace(
    ~freeBaseRules=FreeCellRules.freeBaseRules,
    ~freeRules=FreeCellRules.freeRules,
  )

  module Board = Boards.FRT
})
