open Common
open Packer

module Game = GameBase.Create({
  include Bases.AgnesBernauer

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)

    let beak = deckToDeal->ArrayAux.popN(1)
    let tableau = [
      deckToDeal->ArrayAux.popN(1),
      deckToDeal->ArrayAux.popN(2),
      deckToDeal->ArrayAux.popN(3),
      deckToDeal->ArrayAux.popN(4),
      deckToDeal->ArrayAux.popN(5),
      deckToDeal->ArrayAux.popN(6),
      deckToDeal->ArrayAux.popN(7),
    ]
    let free = [
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
      deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
    ]
    let stock = [
      deckToDeal->ArrayAux.popN(2)->Card.hideAfter(0),
      deckToDeal->ArrayAux.popN(7)->Card.hideAfter(0),
      deckToDeal->ArrayAux.popN(7)->Card.hideAfter(0),
    ]

    let game = {
      tableau,
      foundations: [beak, [], [], []],
      foundations2: [],
      waste: [],
      free,
      stock,
    }
    (shuffledDeck, game)
  }

  let foundationBaseCheck = (game: game, dragPile: dragPile, i) => {
    let justOne = dragPile->Array.length == 1
    let noChildren = game.foundations->Array.getUnsafe(i)->Array.length == 0
    let dragPileBase = dragPile->Array.getUnsafe(0)
    let beak = game.foundations->Array.getUnsafe(0)->Array.getUnsafe(0)
    noChildren && justOne && dragPileBase.card.rank == beak.card.rank
  }

  let foundationBaseRules = (i): staticSpace => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        if foundationBaseCheck(game, dragPile, i) {
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
        let beak = game.foundations->Array.getUnsafe(0)->Array.getUnsafe(0)

        if noChildren && Card.rankIsAboveCyclic(beak, dragPileBase) {
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

  let forEachSpace = Bases.AgnesBernauer.makeForEachSpace(
    ~stockRules=Rules.DealAll.stockRules,
    ~foundationBaseRules,
    ~tableauBaseRules,
    ~freeBaseRules=Rules.FreeCell.freeBaseRules,
    ~freeRules=Rules.FreeCell.freeRules,
  )

  module Board = Boards.SFTR
})
