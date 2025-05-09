open Common
open Packer

module Game = GameBase.Create({
  include Bases.AgnesSorel

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled
    let deckToDeal = ref(shuffledDeck)
    let beak = deckToDeal->ArrayAux.popN(1)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(1),
          deckToDeal->ArrayAux.popN(2),
          deckToDeal->ArrayAux.popN(3),
          deckToDeal->ArrayAux.popN(4),
          deckToDeal->ArrayAux.popN(5),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(7),
        ],
        foundations: [beak, [], [], []],
        stock: [deckToDeal.contents->Card.hideAfter(0)],
        waste: [],
        free: [],
      },
    )
  }

  let foundationBaseCheck = (game: game, dragPile: dragPile, i) => {
    let justOne = dragPile->Array.length == 1
    let noChildren = game.foundations->Array.getUnsafe(i)->Array.length == 0
    let dragPileBase = dragPile->Array.getUnsafe(0)

    noChildren &&
    justOne &&
    dragPileBase.card.rank == (game.foundations->Array.getUnsafe(0)->Array.getUnsafe(0)).card.rank
  }

  let foundationBaseRules = (i): staticSpace => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        if foundationBaseCheck(game, dragPile, i) {
          Some({
            ...game,
            piles: game.piles->GameCommons.flipLastUp,
            foundations: game.foundations->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
      onClick: _ => None,
    }
  }

  let forEachSpace = Bases.AgnesSorel.makeForEachSpace(
    ~stockRules=Spider.SpiderRules.stockRules,
    ~foundationBaseRules,
  )

  module Board = Boards.Spider
})
