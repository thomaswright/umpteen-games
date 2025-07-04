open Common
open Packer

let arePair = (a: Card.sides, b: Card.sides) => {
  switch a.card.rank {
  | RA if b.card.rank == R10 => true
  | R2 if b.card.rank == R9 => true
  | R3 if b.card.rank == R8 => true
  | R4 if b.card.rank == R7 => true
  | R5 if b.card.rank == R6 => true
  | R6 if b.card.rank == R5 => true
  | R7 if b.card.rank == R4 => true
  | R8 if b.card.rank == R3 => true
  | R9 if b.card.rank == R2 => true
  | R10 if b.card.rank == RA => true
  | RJ if b.card.rank == RJ => true
  | RQ if b.card.rank == RK => true
  | RK if b.card.rank == RQ => true
  | _ => false
  }
}

module Game = GameBase.Create({
  include Bases.GayGordons

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
        foundations: [[]],
        foundations2: [],
        free: [
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
        ],
        stock: [],
        waste: [],
      },
    )
  }

  let tableauBaseRules = (_game, _i): staticSpace => {
    {
      droppedUpon: (_gameRemoved, _dragPile) => None,
      autoProgress: Accept,
      onClick: _ => None,
    }
  }

  let tableauRules = (_game, pile, card: Card.sides, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: j * Common.space,
        z: j + 1,
      },
      baseSpace: Tableau(i),
      dragPile: () => {
        let dragPile = pile->Array.sliceToEnd(~start=j)
        if dragPile->Array.length == 1 {
          Some(dragPile)
        } else {
          None
        }
      },
      autoProgress: () => DoNothing,
      droppedUpon: (game, dragPile) => {
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if isLast && arePair(dragPileBase, card) {
          Some({
            ...game,
            foundations: game.foundations->ArrayAux.update(0, a =>
              Array.concat(a, [card, dragPileBase])
            ),
            tableau: game.tableau->ArrayAux.update(i, stack => {
              stack->Array.slice(~start=0, ~end=stack->Array.length - 1)
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

  let forEachSpace = Bases.GayGordons.makeForEachSpace(
    ~freeBaseRules=Rules.FreeCell.freeBaseRules,
    ~freeRules=Rules.FreeCell.freeRules,
    ~tableauRules,
    ~tableauBaseRules,
    ~foundationBaseRules=Rules.Neutral.foundationBaseRules,
    ~foundationRules=Rules.Neutral.foundationRules,
  )

  module Board = Boards.FRT
})
