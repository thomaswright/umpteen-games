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
