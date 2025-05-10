open Common
open Packer

module Game = GameBase.Create({
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
        foundations2: [],
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
    ~freeBaseRules=Rules.FreeCell.freeBaseRules,
    ~freeRules=Rules.FreeCell.freeRules,
    ~foundationBaseRules,
  )

  module Board = Boards.FRT3
})
