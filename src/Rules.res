open Common
open Packer

module FreeCell = {
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

module DealAll = {
  let stockRules = (_game, _card, i, j): movableSpace => {
    {
      locationAdjustment: {
        x: i * Common.space,
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
            tableau: game.tableau
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

module WasteRotation = {
  let stackedBuildWasteRules = (game: Packer.game, card, i): movableSpace => {
    baseSpace: Waste,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: i + 1,
    },
    dragPile: () => {
      if i == game.waste->Array.length - 1 {
        Some([card])
      } else {
        None
      }
    },
    autoProgress: () => DoNothing,
    droppedUpon: (game, dragPile) => {
      let dragPileBase = dragPile->Array.getUnsafe(0)
      game.waste
      ->ArrayAux.getLast
      ->Option.mapOr(None, wasteTop => {
        if (
          dragPileBase.card.suit == wasteTop.card.suit &&
            (Card.rankIsAboveCyclic(wasteTop, dragPileBase) ||
            Card.rankIsAboveCyclic(dragPileBase, wasteTop))
        ) {
          Some({
            ...game,
            waste: game.waste->Array.concat(dragPile),
          })
        } else {
          None
        }
      })
    },
    onClick: _ => None,
    onStateChange: element => Card.showOrHide(card, element),
  }

  let stackedWasteRules = (game: Packer.game, card, i): movableSpace => {
    baseSpace: Waste,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: i + 1,
    },
    dragPile: () => {
      if i == game.waste->Array.length - 1 {
        Some([card])
      } else {
        None
      }
    },
    autoProgress: () => DoNothing,
    droppedUpon: (_, _) => None,
    onClick: _ => None,
    onStateChange: element => Card.showOrHide(card, element),
  }

  let fannedWasteRules = (game: Packer.game, card, i): movableSpace => {
    baseSpace: Waste,
    locationAdjustment: {
      x: 20 * i,
      y: 0,
      z: i + 1,
    },
    dragPile: () => {
      if i == game.waste->Array.length - 1 {
        Some([card])
      } else {
        None
      }
    },
    autoProgress: () => DoNothing,
    droppedUpon: (_, _) => None,
    onClick: _ => None,
    onStateChange: element => Card.showOrHide(card, element),
  }

  let stockRules = (_game, card, _i, j): movableSpace => {
    baseSpace: Stock,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: j + 1,
    },
    dragPile: () => None,
    autoProgress: () => DoNothing,
    droppedUpon: (_, _) => None,
    onClick: game => {
      let realStock = game.stock->Array.getUnsafe(0)
      Some({
        ...game,
        stock: game.stock->ArrayAux.update(0, v =>
          v->Array.slice(~start=0, ~end=realStock->Array.length - 1)
        ),
        waste: game.waste->Array.concat(
          realStock
          ->Array.sliceToEnd(~start=realStock->Array.length - 1)
          ->Card.showAfter(0),
        ),
      })
    },
    onStateChange: element => Card.showOrHide(card, element),
  }

  let stockBaseRules = (): staticSpace => {
    autoProgress: DoNothing,
    droppedUpon: (_game, _dragPile) => {
      None
    },
    onClick: game => {
      Some({
        ...game,
        stock: [game.waste->Array.toReversed->Card.hideAfter(0)],
        waste: [],
      })
    },
  }
}

module Neutral = {
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
}
