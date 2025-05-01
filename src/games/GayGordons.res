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
        piles: [
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
        free: [
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
          deckToDeal->ArrayAux.popN(1)->Array.getUnsafe(0)->Some,
        ],
        stock: [],
        waste: [],
      },
    )
  }

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
      autoProgress: () => DoNothing,
      dragPile: () => Some([card]),
      droppedUpon: (_game, _dragPile) => None,
      onClick: _ => None,
      onStateChange: _ => (),
    }
  }

  let pileBaseRules = (_game, _i): staticSpace => {
    {
      droppedUpon: (_gameRemoved, _dragPile) => None,
      autoProgress: Accept,
      onClick: _ => None,
    }
  }

  let pileRules = (_game, pile, card: Card.sides, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: j * 20,
        z: j + 1,
      },
      baseSpace: Pile(i),
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
            piles: game.piles->ArrayAux.update(i, stack => {
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

  let foundationBaseRules = (_): staticSpace => {
    {
      autoProgress: DoNothing,
      droppedUpon: (_game, _dragPile) => None,
      onClick: _ => None,
    }
  }

  let foundationRules = (_game, _pile, card, i, j): movableSpace => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: j + 1,
      },
      baseSpace: Foundation(i),
      dragPile: () => None,
      autoProgress: () => DoNothing,
      droppedUpon: (_game, _dragPile) => None,
      onClick: _ => None,
      onStateChange: element => Card.showOrHide(card, element),
    }
  }

  let forEachSpace = Bases.GayGordons.makeForEachSpace(
    ~freeBaseRules,
    ~freeRules,
    ~pileRules,
    ~pileBaseRules,
    ~foundationBaseRules,
    ~foundationRules,
  )

  module Board = Boards.FreeCell
})
