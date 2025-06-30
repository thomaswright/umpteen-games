open Common
open Packer

module Game = GameBase.Create({
  include Bases.GrandfathersClock

  let initiateGame = (): (array<Card.sides>, Packer.game) => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    deckToDeal :=
      deckToDeal.contents->Array.filterMap(v => {
        let c = v.card
        if (
          (c.rank == R2 && c.suit == Hearts) ||
          c.rank == R3 && c.suit == Spades ||
          c.rank == R4 && c.suit == Diamonds ||
          c.rank == R5 && c.suit == Clubs ||
          c.rank == R6 && c.suit == Hearts ||
          c.rank == R7 && c.suit == Spades ||
          c.rank == R8 && c.suit == Diamonds ||
          c.rank == R9 && c.suit == Clubs ||
          c.rank == R10 && c.suit == Hearts ||
          c.rank == RJ && c.suit == Spades ||
          c.rank == RQ && c.suit == Diamonds ||
          (c.rank == RK && c.suit == Clubs)
        ) {
          None
        } else {
          Some(v)
        }
      })

    let tableau = [
      deckToDeal->ArrayAux.popN(5),
      deckToDeal->ArrayAux.popN(5),
      deckToDeal->ArrayAux.popN(5),
      deckToDeal->ArrayAux.popN(5),
      deckToDeal->ArrayAux.popN(5),
      deckToDeal->ArrayAux.popN(5),
      deckToDeal->ArrayAux.popN(5),
      deckToDeal->ArrayAux.popN(5),
    ]
    (
      shuffledDeck,
      {
        tableau,
        foundations: [
          [{card: {rank: R10, suit: Hearts, deck: 0}, hidden: false}],
          [{card: {rank: RJ, suit: Spades, deck: 0}, hidden: false}],
          [{card: {rank: RQ, suit: Diamonds, deck: 0}, hidden: false}],
          [{card: {rank: RK, suit: Clubs, deck: 0}, hidden: false}],
          [{card: {rank: R2, suit: Hearts, deck: 0}, hidden: false}],
          [{card: {rank: R3, suit: Spades, deck: 0}, hidden: false}],
          [{card: {rank: R4, suit: Diamonds, deck: 0}, hidden: false}],
          [{card: {rank: R5, suit: Clubs, deck: 0}, hidden: false}],
          [{card: {rank: R6, suit: Hearts, deck: 0}, hidden: false}],
          [{card: {rank: R7, suit: Spades, deck: 0}, hidden: false}],
          [{card: {rank: R8, suit: Diamonds, deck: 0}, hidden: false}],
          [{card: {rank: R9, suit: Clubs, deck: 0}, hidden: false}],
        ],
        foundations2: [],
        stock: [],
        waste: [],
        free: [],
      },
    )
  }

  let foundationRules = (game, pile, card, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: j + 1,
      },
      baseSpace: Foundation(i),
      dragPile: () => {
        if j == game.foundations->Array.length - 1 {
          Some([card])
        } else {
          None
        }
      },
      autoProgress: () => DoNothing,
      droppedUpon: (game, dragPile) => {
        let justOne = dragPile->Array.length == 1
        let dragPileBase = dragPile->Array.getUnsafe(0)
        if (
          isLast &&
          justOne &&
          dragPileBase.card.suit == card.card.suit &&
          Card.rankIsAboveCyclic(dragPileBase, card) &&
          dragPileBase.card.rank != Card.getNumberedRankCyclic(i + 1)
        ) {
          Some({
            ...game,
            tableau: game.tableau->GameCommons.flipLastUp,
            foundations: game.foundations->Array.map(stack => {
              stack->ArrayAux.insertAfter(card, dragPile)
            }),
          })
        } else {
          None
        }
      },
      onClick: _ => None,
      onStateChange: element => Card.showOrHide(card, element),
    }
  }

  let forEachSpace = makeForEachSpace(~foundationRules)

  module Board = Boards.FT_Clock
})
