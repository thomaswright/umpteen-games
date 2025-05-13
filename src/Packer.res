open Webapi.Dom
open Common

@decco
type space =
  Card(Card.card) | Foundation(int) | Foundation2(int) | Tableau(int) | Waste | Stock | Free(int)

@decco
type game = {
  tableau: array<array<Card.sides>>,
  foundations: array<array<Card.sides>>,
  foundations2: array<array<Card.sides>>,
  stock: array<array<Card.sides>>,
  waste: array<Card.sides>,
  free: array<option<Card.sides>>,
}
type dragPile = array<Card.sides>
type movableSpace = GameBase.movableSpace<game, space, dragPile>
type staticSpace = GameBase.staticSpace<game, dragPile>

type stack =
  | AltColor
  | AnySuit
  | OneSuit
  | EitherWayOneSuit
  | CyclicOneSuit
  | CyclicAnySuit
  | CyclicAltColor
  | CyclicSameColor
  | NoDrop

type size = AnySize | FreeSize | JustOne
type depot = SpecificDepot(Card.rank) | AnyDepot
type foundation = ByOne | ByAll | ByOneCyclicOneSuit | ByOneCyclicAnySuit | NoFoundation

type spec = {drop: stack, drag: stack, size: size, depot: depot, foundation: foundation}

let onlyFoundationWinCheck = (game: game) => {
  game.tableau->Array.every(pile => pile->Array.length == 0) &&
  game.free->Array.every(Option.isNone) &&
  game.stock->Array.length == 0 &&
  game.waste->Array.length == 0
}

module type PackerRules = {
  let spec: spec
}

let getSpace = element => {
  switch element->Element.id->Js.Json.parseExn->space_decode {
  | Ok(d) => Some(d)
  | _ => None
  }
}

let spaceToString = space => {
  space->space_encode->Js.Json.stringify
}

module Make = (PackerRules: PackerRules) => {
  type space = space
  @decco
  type game = game

  let getSpace = getSpace
  let spaceToString = spaceToString
  type dragPile = dragPile
  @decco
  type deck = array<Card.sides>

  let winCheck = onlyFoundationWinCheck

  let dropCheck = (isLast, dragPile, card) => {
    let dragPileBase = dragPile->Array.getUnsafe(0)
    isLast &&
    switch PackerRules.spec.drop {
    | AltColor =>
      Card.rankIsAbove(card, dragPileBase) && dragPileBase->Card.color != card->Card.color
    | AnySuit => Card.rankIsAbove(card, dragPileBase)
    | OneSuit => Card.rankIsAbove(card, dragPileBase) && dragPileBase.card.suit == card.card.suit
    | CyclicOneSuit =>
      Card.rankIsAboveCyclic(card, dragPileBase) && dragPileBase.card.suit == card.card.suit
    | CyclicSameColor =>
      Card.rankIsAboveCyclic(card, dragPileBase) && dragPileBase->Card.color == card->Card.color
    | CyclicAltColor =>
      Card.rankIsAboveCyclic(card, dragPileBase) && dragPileBase->Card.color != card->Card.color
    | CyclicAnySuit => Card.rankIsAboveCyclic(card, dragPileBase)
    | EitherWayOneSuit =>
      (Card.rankIsAbove(card, dragPileBase) || Card.rankIsAbove(dragPileBase, card)) &&
        dragPileBase.card.suit == card.card.suit
    | NoDrop => false
    }
  }

  let dragCheck = dragPile =>
    switch PackerRules.spec.drag {
    | AltColor => dragPile->GameCommons.decAltColorValidation
    | OneSuit => dragPile->GameCommons.decOneSuitValidation
    | AnySuit => true
    | CyclicOneSuit => dragPile->GameCommons.decCyclicOneSuitValidation
    | CyclicAnySuit => dragPile->GameCommons.decCyclicAnySuitValidation
    | CyclicSameColor => dragPile->GameCommons.decCyclicSameColorValidation
    | CyclicAltColor => dragPile->GameCommons.decCyclicAltColorValidation
    | EitherWayOneSuit => dragPile->GameCommons.eitherWayOneSuitValidation
    | NoDrop => false
    }

  let dragSizeCheck = (game: game, dragPile: dragPile) => {
    let freeCellCount =
      game.tableau->Array.filter(pile => pile->Array.length == 0)->Array.length +
        game.free->Array.filter(Option.isNone)->Array.length
    switch PackerRules.spec.size {
    | AnySize => true
    | FreeSize => freeCellCount >= dragPile->Array.length - 1
    | JustOne => dragPile->Array.length == 1
    }
  }

  let pileBaseCheck = (game: game, dragPile: dragPile, i) => {
    let dragPileBase = dragPile->Array.getUnsafe(0)
    let noChildren = game.tableau->Array.getUnsafe(i)->Array.length == 0

    switch PackerRules.spec.depot {
    | SpecificDepot(rank) => noChildren && dragPileBase.card.rank == rank
    | AnyDepot => noChildren
    }
  }

  let foundationCheck = (dragPile: dragPile, card: Card.sides) => {
    let justOne = dragPile->Array.length == 1
    let dragPileBase = dragPile->Array.getUnsafe(0)

    switch PackerRules.spec.foundation {
    | ByOneCyclicAnySuit => justOne && Card.rankIsAboveCyclic(dragPileBase, card)
    | ByOneCyclicOneSuit =>
      justOne &&
      dragPileBase.card.suit == card.card.suit &&
      Card.rankIsAboveCyclic(dragPileBase, card)
    | ByOne =>
      justOne && dragPileBase.card.suit == card.card.suit && Card.rankIsAbove(dragPileBase, card)
    | ByAll => false
    | NoFoundation => false
    }
  }

  let foundationBaseCheck = (game: game, dragPile: dragPile, i) => {
    let justOne = dragPile->Array.length == 1
    let fullStack = dragPile->Array.length == 13
    let noChildren = game.foundations->Array.getUnsafe(i)->Array.length == 0
    let valid = dragPile->GameCommons.decOneSuitValidation
    let dragPileBase = dragPile->Array.getUnsafe(0)

    switch PackerRules.spec.foundation {
    | ByOne => noChildren && justOne && dragPileBase.card.rank == RA
    | ByAll => noChildren && fullStack && valid
    | ByOneCyclicOneSuit => noChildren && justOne && dragPileBase.card.rank == RA
    | ByOneCyclicAnySuit => noChildren && justOne && dragPileBase.card.rank == RA
    | NoFoundation => false
    }
  }

  let foundation2Check = (dragPile: dragPile, card: Card.sides) => {
    let justOne = dragPile->Array.length == 1
    let dragPileBase = dragPile->Array.getUnsafe(0)

    switch PackerRules.spec.foundation {
    | ByOneCyclicAnySuit => justOne && Card.rankIsAboveCyclic(card, dragPileBase)
    | ByOneCyclicOneSuit =>
      justOne &&
      dragPileBase.card.suit == card.card.suit &&
      Card.rankIsAboveCyclic(card, dragPileBase)
    | ByOne =>
      justOne && dragPileBase.card.suit == card.card.suit && Card.rankIsAbove(card, dragPileBase)
    | ByAll => false
    | NoFoundation => false
    }
  }

  let foundation2BaseCheck = (game: game, dragPile: dragPile, i) => {
    let justOne = dragPile->Array.length == 1
    let fullStack = dragPile->Array.length == 13
    let noChildren = game.foundations2->Array.getUnsafe(i)->Array.length == 0
    let valid = dragPile->GameCommons.decOneSuitValidation
    let dragPileBase = dragPile->Array.getUnsafe(0)

    switch PackerRules.spec.foundation {
    | ByOne => noChildren && justOne && dragPileBase.card.rank == RK
    | ByAll => noChildren && fullStack && valid
    | ByOneCyclicOneSuit => noChildren && justOne && dragPileBase.card.rank == RK
    | ByOneCyclicAnySuit => noChildren && justOne && dragPileBase.card.rank == RK
    | NoFoundation => false
    }
  }

  let applyLiftToDragPile = (dragPile: dragPile, lift) => {
    dragPile->Array.forEachWithIndex((v, j) => {
      lift(Card(v.card), j)
    })
  }

  let applyMoveToDragPile = (dragPile: dragPile, move) => {
    dragPile->Array.forEachWithIndex((v, j) => {
      move(Card(v.card), 0, j * Common.space)
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
      foundations2: game.foundations2->Array.map(removeDragPile),
      tableau: game.tableau->Array.map(removeDragPile),
      stock: game.stock->Array.map(removeDragPile),
      waste: game.waste->removeDragPile,
      free: game.free->Array.map(card => {
        card->Option.flatMap(card =>
          dragPile->Array.some(dCard => card == dCard) ? None : Some(card)
        )
      }),
    }
  }

  let tableauBaseRules = (game, i): staticSpace => {
    {
      droppedUpon: (gameRemoved, dragPile) => {
        if pileBaseCheck(game, dragPile, i) {
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

  let tableauRules = (game, pile, card, i, j): movableSpace => {
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
        if dragCheck(dragPile) && dragSizeCheck(game, dragPile) {
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
            tableau: game.tableau
            ->Array.map(stack => {
              stack->ArrayAux.insertAfter(card, dragPile)
            })
            ->GameCommons.flipLastUp,
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
      autoProgress: () => Seek,
      droppedUpon: (game, dragPile) => {
        if isLast && foundationCheck(dragPile, card) {
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

  let foundation2BaseRules = (i): staticSpace => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        if foundation2BaseCheck(game, dragPile, i) {
          Some({
            ...game,
            tableau: game.tableau->GameCommons.flipLastUp,
            foundations2: game.foundations2->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
      onClick: _ => None,
    }
  }

  let foundation2Rules = (game, pile, card, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: j + 1,
      },
      baseSpace: Foundation2(i),
      dragPile: () => {
        if j == game.foundations2->Array.length - 1 {
          Some([card])
        } else {
          None
        }
      },
      autoProgress: () => Seek,
      droppedUpon: (gameRemoved, dragPile) => {
        if isLast && foundation2Check(dragPile, card) {
          Some({
            ...gameRemoved,
            tableau: gameRemoved.tableau->GameCommons.flipLastUp,
            foundations2: gameRemoved.foundations2->Array.map(stack => {
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

  let wasteRules = (_game, _card, i): movableSpace => {
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

  let stockRules = (_game, _card, _i, j): movableSpace => {
    baseSpace: Stock,
    locationAdjustment: {
      x: 0,
      y: 0,
      z: j + 1,
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

  let freeRules = (_card, i): movableSpace => {
    baseSpace: Free(i),
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
    ~tableauBaseRules=tableauBaseRules,
    ~tableauRules=tableauRules,
    ~foundationBaseRules=foundationBaseRules,
    ~foundationRules=foundationRules,
    ~foundation2BaseRules=foundation2BaseRules,
    ~foundation2Rules=foundation2Rules,
    ~wasteRules=wasteRules,
    ~stockBaseRules=stockBaseRules,
    ~stockRules=stockRules,
    ~freeBaseRules=freeBaseRules,
    ~freeRules=freeRules,
  ) => {
    (game: game, f) => {
      game.tableau->Array.forEachWithIndex((pile, i) => {
        f(Tableau(i), tableauBaseRules(game, i)->GameBase.Static)

        pile->Array.forEachWithIndex((card, j) => {
          f(Card(card.card), tableauRules(game, pile, card, i, j)->Movable)
        })
      })

      game.foundations->Array.forEachWithIndex((foundation, i) => {
        f(Foundation(i), foundationBaseRules(i)->Static)

        foundation->Array.forEachWithIndex((card, j) => {
          f(Card(card.card), foundationRules(game, foundation, card, i, j)->Movable)
        })
      })

      game.foundations2->Array.forEachWithIndex((foundation2, i) => {
        f(Foundation2(i), foundation2BaseRules(i)->Static)

        foundation2->Array.forEachWithIndex((card, j) => {
          f(Card(card.card), foundation2Rules(game, foundation2, card, i, j)->Movable)
        })
      })

      game.waste->Array.forEachWithIndex((card, i) => {
        f(Card(card.card), wasteRules(game, card, i)->Movable)
      })

      game.stock->Array.forEachWithIndex((group, i) => {
        group->Array.forEachWithIndex((card, j) => {
          f(Card(card.card), stockRules(game, card, i, j)->Movable)
        })
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
