open Webapi.Dom
open Common

module FreeCellRules = {
  @decco
  type space = Card(Card.card) | Foundation(int) | Pile(int) | Free(int)

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
    free: array<option<Card.sides>>,
  }

  type movableSpace = GameBase.movableSpace<game, space, dragPile>
  type staticSpace = GameBase.staticSpace<game, dragPile>

  let initiateGame = () => {
    let shuffledDeck = Card.getDeck(0, false)->Array.toShuffled

    let deckToDeal = ref(shuffledDeck)

    (
      shuffledDeck,
      {
        piles: [
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(7),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
          deckToDeal->ArrayAux.popN(6),
        ],
        foundations: [[], [], [], []],
        free: [None, None, None, None],
      },
    )
  }

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.length == 0) &&
      game.free->Array.every(Option.isNone)
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
      free: game.free->Array.map(card => {
        card->Option.flatMap(card =>
          dragPile->Array.some(dCard => card == dCard) ? None : Some(card)
        )
      }),
    }
  }

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

  let pileBaseRules = (i): staticSpace => {
    {
      droppedUpon: (game, dragPile) => {
        let noChildren = game.piles->Array.getUnsafe(i)->Array.length == 0

        if noChildren {
          Some({
            ...game,
            piles: game.piles->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
      autoProgress: Accept,
      onClick: _ => None,
    }
  }

  let pileRules = (game, pile, card, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: j * 20,
        z: j + 1,
      },
      baseSpace: Pile(i),
      dragPile: () => {
        let freeCellCount =
          game.piles->Array.filter(pile => pile->Array.length == 0)->Array.length +
            game.free->Array.filter(Option.isNone)->Array.length
        let dragPile = pile->Array.sliceToEnd(~start=j)
        if (
          dragPile->GameCommons.decAndAltValidation && freeCellCount >= dragPile->Array.length - 1
        ) {
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
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if (
          isLast &&
          Card.rankIsAbove(card, dragPileBase) &&
          dragPileBase->Card.color != card->Card.color
        ) {
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
      onStateChange: _ => (),
    }
  }

  let foundationBaseRules = (i): staticSpace => {
    {
      autoProgress: Seek,
      droppedUpon: (game, dragPile) => {
        let justOne = dragPile->Array.length == 1
        let dragPileBase = dragPile->Array.getUnsafe(0)
        let noChildren = game.foundations->Array.getUnsafe(i)->Array.length == 0

        if noChildren && justOne && dragPileBase.card.rank == RA {
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

  let foundationRules = (game, foundation, card, i, j): movableSpace => {
    let isLast = j == foundation->Array.length - 1

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
        let justOne = dragPile->Array.length == 1
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if (
          isLast &&
          justOne &&
          dragPileBase.card.suit == card.card.suit &&
          Card.rankIsBelow(card, dragPileBase)
        ) {
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
      onStateChange: _ => (),
    }
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
      autoProgress: () => Send([card]),
      dragPile: () => Some([card]),
      droppedUpon: (_game, _dragPile) => None,
      onClick: _ => None,
      onStateChange: _ => (),
    }
  }

  let forEachSpace: GameBase.forEachSpace<game, space, dragPile> = (game: game, f) => {
    game.piles->Array.forEachWithIndex((pile, i) => {
      f(Pile(i), pileBaseRules(i)->GameBase.Static)

      pile->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), pileRules(game, pile, card, i, j)->Movable)
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      f(Foundation(i), foundationBaseRules(i)->Static)

      foundation->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), foundationRules(game, foundation, card, i, j)->Movable)
      })
    })

    game.free->Array.forEachWithIndex((card, i) => {
      f(Free(i), freeBaseRules(i)->Static)

      card->Option.mapOr((), card => {
        f(Card(card.card), freeRules(card, i)->Movable)
      })
    })
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

  module StandardBoard = {
    @react.component
    let make = (
      ~setRef,
      ~onMouseDown as _,
      ~setGame as _,
      ~moveToState as _,
      ~autoProgress as _,
      ~game as _,
      ~undo as _,
      ~isWin as _,
      ~onClick as _,
    ) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div className="flex flex-row gap-3">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Free(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
                className=" bg-black opacity-20   rounded w-14 h-20"
              />
            })
            ->React.array}
          </div>
          <div className="flex flex-row gap-3 ml-10">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Foundation(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
                className=" bg-white opacity-10  rounded w-14 h-20"
              />
            })
            ->React.array}
          </div>
        </div>
        <div />
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], [], [], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Pile(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
              className=" bg-black opacity-20   rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </React.Fragment>
    }
  }
}

module OneDeck = GameBase.Create({
  include FreeCellRules

  module Board = FreeCellRules.StandardBoard
})

module TwoDeck = GameBase.Create({
  include FreeCellRules

  module Board = {
    @react.component
    let make = (
      ~setRef,
      ~onMouseDown as _,
      ~setGame as _,
      ~moveToState as _,
      ~autoProgress as _,
      ~game as _,
      ~undo as _,
      ~isWin as _,
      ~onClick as _,
    ) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div className="flex flex-col gap-3">
            <div className="flex flex-row gap-3">
              {[[], [], [], []]
              ->Array.mapWithIndex((_, i) => {
                <div
                  key={Free(i)->spaceToString}
                  ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
                  className="   bg-black opacity-20  rounded w-14 h-20"
                />
              })
              ->React.array}
            </div>
            <div className="flex flex-row gap-3">
              {[[], [], [], []]
              ->Array.mapWithIndex((_, i) => {
                <div
                  key={Free(i + 4)->spaceToString}
                  ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i + 4)))}
                  className="   bg-black opacity-20  rounded w-14 h-20"
                />
              })
              ->React.array}
            </div>
          </div>
          <div className="flex flex-col gap-3">
            <div className="flex flex-row gap-3 ml-10">
              {[[], [], [], []]
              ->Array.mapWithIndex((_, i) => {
                <div
                  key={Free(i)->spaceToString}
                  ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
                  className="   bg-white opacity-10  rounded w-14 h-20"
                />
              })
              ->React.array}
            </div>
            <div className="flex flex-row gap-3 ml-10">
              {[[], [], [], []]
              ->Array.mapWithIndex((_, i) => {
                <div
                  key={Free(i + 4)->spaceToString}
                  ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i + 4)))}
                  className="   bg-white opacity-10  rounded w-14 h-20"
                />
              })
              ->React.array}
            </div>
          </div>
        </div>
        <div />
        <div className="flex flex-row gap-3 mt-5">
          {[[], [], [], [], [], [], [], [], [], []]
          ->Array.mapWithIndex((_, i) => {
            <div
              key={Pile(i)->spaceToString}
              ref={ReactDOM.Ref.callbackDomRef(setRef(Pile(i)))}
              className=" bg-black opacity-20  rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </React.Fragment>
    }
  }
})

module BakersGame = GameBase.Create({
  include FreeCellRules

  let pileBaseRules = (i): staticSpace => {
    {
      droppedUpon: (game, dragPile) => {
        let noChildren = game.piles->Array.getUnsafe(i)->Array.length == 0
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if noChildren && dragPileBase.card.rank == RK {
          Some({
            ...game,
            piles: game.piles->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
      autoProgress: Accept,
      onClick: _ => None,
    }
  }

  let pileRules = (game, pile, card, i, j): movableSpace => {
    let isLast = j == pile->Array.length - 1

    {
      locationAdjustment: {
        x: 0,
        y: j * 20,
        z: j + 1,
      },
      baseSpace: Pile(i),
      dragPile: () => {
        let freeCellCount =
          game.piles->Array.filter(pile => pile->Array.length == 0)->Array.length +
            game.free->Array.filter(Option.isNone)->Array.length
        let dragPile = pile->Array.sliceToEnd(~start=j)
        if dragPile->GameCommons.decValidation && freeCellCount >= dragPile->Array.length - 1 {
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
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if (
          isLast && Card.rankIsAbove(card, dragPileBase) && dragPileBase.card.suit == card.card.suit
        ) {
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
      onStateChange: _ => (),
    }
  }

  let forEachSpace: GameBase.forEachSpace<game, space, dragPile> = (game: game, f) => {
    game.piles->Array.forEachWithIndex((pile, i) => {
      f(Pile(i), pileBaseRules(i)->GameBase.Static)

      pile->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), pileRules(game, pile, card, i, j)->Movable)
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      f(Foundation(i), foundationBaseRules(i)->Static)

      foundation->Array.forEachWithIndex((card, j) => {
        f(Card(card.card), foundationRules(game, foundation, card, i, j)->Movable)
      })
    })

    game.free->Array.forEachWithIndex((card, i) => {
      f(Free(i), freeBaseRules(i)->Static)

      card->Option.mapOr((), card => {
        f(Card(card.card), freeRules(card, i)->Movable)
      })
    })
  }

  module Board = FreeCellRules.StandardBoard
})
