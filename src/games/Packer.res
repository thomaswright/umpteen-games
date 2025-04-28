open Webapi.Dom
open Common

type stack = AltSuit | AnySuit | OneSuit
type size = AnySize | FreeSize
type depot = KingDepot | AnyDepot
type foundation = ByOne | ByAll

type spec = {drop: stack, drag: stack, size: size, depot: depot, foundation: foundation}

module type PackerRules = {
  let spec: spec
}

module Make = (PackerRules: PackerRules) => {
  @decco
  type space = Card(Card.card) | Foundation(int) | Pile(int) | Waste | Stock | Free(int)

  let getSpace = element => {
    switch element->Element.id->Js.Json.parseExn->space_decode {
    | Ok(d) => Some(d)
    | _ => None
    }
  }

  let spaceToString = space => {
    space->space_encode->Js.Json.stringify
  }

  type dragPile = array<Card.sides>
  @decco
  type deck = array<Card.sides>
  @decco
  type game = {
    piles: array<array<Card.sides>>,
    foundations: array<array<Card.sides>>,
    stock: array<array<Card.sides>>,
    waste: array<Card.sides>,
    free: array<option<Card.sides>>,
  }

  let dropCheck = (isLast, dragPile, card) => {
    let dragPileBase = dragPile->Array.getUnsafe(0)

    switch PackerRules.spec.drop {
    | AltSuit =>
      isLast && Card.rankIsAbove(card, dragPileBase) && dragPileBase->Card.color != card->Card.color
    | AnySuit => isLast && Card.rankIsAbove(card, dragPileBase)
    | OneSuit =>
      isLast && Card.rankIsAbove(card, dragPileBase) && dragPileBase.card.suit == card.card.suit
    }
  }

  let dragCheck = dragPile =>
    switch PackerRules.spec.drag {
    | AltSuit => dragPile->GameCommons.decAndAltValidation
    | OneSuit => dragPile->GameCommons.decValidation
    | AnySuit => true
    }

  let dragSizeCheck = (game: game, dragPile: dragPile) => {
    let freeCellCount =
      game.piles->Array.filter(pile => pile->Array.length == 0)->Array.length +
        game.free->Array.filter(Option.isNone)->Array.length

    switch PackerRules.spec.size {
    | AnySize => true
    | FreeSize => freeCellCount >= dragPile->Array.length - 1
    }
  }

  let pileBaseCheck = (game: game, dragPile: dragPile, i) => {
    let dragPileBase = dragPile->Array.getUnsafe(0)
    let noChildren = game.piles->Array.getUnsafe(i)->Array.length == 0

    switch PackerRules.spec.depot {
    | KingDepot => noChildren && dragPileBase.card.rank == RK
    | AnyDepot => noChildren
    }
  }

  let foundationBaseCheck = (game: game, dragPile: dragPile, i) => {
    let justOne = dragPile->Array.length == 1
    let fullStack = dragPile->Array.length == 13
    let noChildren = game.foundations->Array.getUnsafe(i)->Array.length == 0
    let valid = dragPile->GameCommons.decValidation
    let dragPileBase = dragPile->Array.getUnsafe(0)

    switch PackerRules.spec.foundation {
    | ByOne => noChildren && justOne && dragPileBase.card.rank == RA
    | ByAll => noChildren && fullStack && valid
    }
  }

  let foundationCheck = (dragPile: dragPile, card: Card.sides, i) => {
    let justOne = dragPile->Array.length == 1
    let dragPileBase = dragPile->Array.getUnsafe(0)

    switch PackerRules.spec.foundation {
    | ByOne =>
      justOne && dragPileBase.card.suit == card.card.suit && Card.rankIsBelow(card, dragPileBase)
    | ByAll => false
    }
  }

  type movableSpace = GameBase.movableSpace<game, space, dragPile>
  type staticSpace = GameBase.staticSpace<game, dragPile>

  let applyLiftToDragPile = (dragPile: dragPile, lift) => {
    dragPile->Array.forEachWithIndex((v, j) => {
      lift(Card(v.card), j)
    })
  }

  let applyMoveToDragPile = (dragPile: dragPile, move) => {
    dragPile->Array.forEachWithIndex((v, j) => {
      move(Card(v.card), 0, j * 20)
    })
  }

  let removeDragFromGame = (game: game, dragPile: dragPile) => {
    let dragPileSet = dragPile->Set.fromArray
    let removeDragPile = x =>
      x->Array.filter(sCard => {
        !(dragPileSet->Set.has(sCard))
      })

    {
      foundations: game.foundations->Array.map(removeDragPile),
      piles: game.piles->Array.map(removeDragPile),
      stock: game.stock->Array.map(removeDragPile),
      waste: game.waste->removeDragPile,
      free: game.free->Array.map(card => {
        card->Option.flatMap(card =>
          dragPile->Array.some(dCard => card == dCard) ? None : Some(card)
        )
      }),
    }
  }

  let pileBaseRules = (game, i): staticSpace => {
    {
      droppedUpon: (gameRemoved, dragPile) => {
        if pileBaseCheck(game, dragPile, i) {
          Some({
            ...gameRemoved,
            piles: gameRemoved.piles->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
      autoProgress: Accept,
      onClick: _ => None,
    }
  }

  let pileRules = (pile, card, i, j): movableSpace => {
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
        if dragCheck(dragPile) {
          Some(dragPile)
        } else {
          None
        }
      },
      autoProgress: () => {
        if isLast {
          SendOrAccept([card])
        } else {
          DoNothing
        }
      },
      droppedUpon: (game, dragPile) => {
        if dropCheck(isLast, dragPile, card) {
          Some({
            ...game,
            piles: game.piles->Array.map(stack => {
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

  let foundationBaseRules = (i): staticSpace => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        if foundationBaseCheck(game, dragPile, i) {
          Some({
            ...game,
            foundations: game.foundations->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
      onClick: _ => None,
    }
  }

  let foundationRules = (game, card, i, j): movableSpace => {
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
      autoProgress: () => Seek,
      droppedUpon: (game, dragPile) => {
        if foundationBaseCheck(game, dragPile, i) {
          Some({
            ...game,
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

  let wasteRules = (game, card, i): movableSpace => {
    baseSpace: Waste,
    locationAdjustment: {
      x: 20 * i,
      y: 0,
      z: i + 1,
    },
    dragPile: () => None,
    autoProgress: () => DoNothing,
    droppedUpon: (_, _) => None,
    onClick: _ => None,
    onStateChange: _ => (),
  }

  let stockRules = (card, i): movableSpace => {
    baseSpace: Stock,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: i + 1,
    },
    dragPile: () => None,
    autoProgress: () => DoNothing,
    droppedUpon: (_, _) => None,
    onClick: _ => None,
    onStateChange: _ => (),
  }

  let stockBaseRules = (): staticSpace => {
    autoProgress: DoNothing,
    droppedUpon: (_, _) => None,
    onClick: _ => None,
  }

  let freeRules = (card, i): movableSpace => {
    baseSpace: Stock,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: i + 1,
    },
    dragPile: () => None,
    autoProgress: () => DoNothing,
    droppedUpon: (_, _) => None,
    onClick: _ => None,
    onStateChange: _ => (),
  }

  let freeBaseRules = (_): staticSpace => {
    autoProgress: DoNothing,
    droppedUpon: (_, _) => None,
    onClick: _ => None,
  }

  let makeForEachSpace = (
    ~pileBaseRules=pileBaseRules,
    ~pileRules=pileRules,
    ~foundationBaseRules=foundationBaseRules,
    ~foundationRules=foundationRules,
    ~wasteRules=wasteRules,
    ~stockBaseRules=stockBaseRules,
    ~stockRules=stockRules,
    ~freeBaseRules=freeBaseRules,
    ~freeRules=freeRules,
  ) => {
    (game: game, f) => {
      game.piles->Array.forEachWithIndex((pile, i) => {
        f(Pile(i), pileBaseRules(game, i)->GameBase.Static)

        pile->Array.forEachWithIndex((card, j) => {
          f(Card(card.card), pileRules(pile, card, i, j)->Movable)
        })
      })

      game.foundations->Array.forEachWithIndex((foundation, i) => {
        f(Foundation(i), foundationBaseRules(i)->Static)

        foundation->Array.forEachWithIndex((card, j) => {
          f(Card(card.card), foundationRules(game, card, i, j)->Movable)
        })
      })

      game.waste->Array.forEachWithIndex((card, i) => {
        f(Card(card.card), wasteRules(game, card, i)->Movable)
      })

      game.stock
      ->Array.getUnsafe(0)
      ->Array.forEachWithIndex((card, i) => {
        f(Card(card.card), stockRules(card, i)->Movable)
      })

      f(Stock, stockBaseRules()->Static)

      game.free->Array.forEachWithIndex((card, i) => {
        f(Free(i), freeBaseRules(i)->Static)

        card->Option.mapOr((), card => {
          f(Card(card.card), freeRules(card, i)->Movable)
        })
      })
    }
  }

  module AllCards = {
    @react.component
    let make = (~setRef, ~onMouseDown, ~deck) => {
      <React.Fragment>
        {deck
        ->Array.map(card => {
          <Card.Display
            card={card}
            key={Card(card.card)->spaceToString}
            id={Card(card.card)->spaceToString}
            cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card.card)))}
            onMouseDown={onMouseDown}
          />
        })
        ->React.array}
      </React.Fragment>
    }
  }
}
