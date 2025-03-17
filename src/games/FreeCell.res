open Webapi.Dom
open Common
open GameBase

module GameRules: GameBase.GameRules = {
  let shuffledDeck = Card.getShuffledDeck()

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

  type dragPile = array<Card.card>

  type game = {
    piles: array<array<Card.card>>,
    foundations: array<array<Card.card>>,
    free: array<option<Card.card>>,
  }

  type movableSpace = GameBase.movableSpace<game, space, dragPile>
  type staticSpace = GameBase.staticSpace<game, dragPile>

  let dragPileValidation = dragPile => {
    let (dragPileIsValid, _) =
      dragPile
      ->Array.toReversed
      ->Array.reduce((true, None), ((isStillValid, onTop), onBottom) => {
        !isStillValid
          ? (false, None)
          : switch (onTop, onBottom) {
            | (Some(onTop), onBottom) => (
                Card.rankIsBelow(onTop, onBottom) && onTop->Card.color != onBottom->Card.color,
                Some(onBottom),
              )
            | _ => (true, Some(onBottom))
            }
      })
    dragPileIsValid
  }

  let initiateGame = () => {
    {
      piles: [
        shuffledDeck->Array.slice(~start=0, ~end=7),
        shuffledDeck->Array.slice(~start=7, ~end=14),
        shuffledDeck->Array.slice(~start=14, ~end=21),
        shuffledDeck->Array.slice(~start=21, ~end=28),
        shuffledDeck->Array.slice(~start=28, ~end=34),
        shuffledDeck->Array.slice(~start=34, ~end=40),
        shuffledDeck->Array.slice(~start=40, ~end=46),
        shuffledDeck->Array.slice(~start=46, ~end=52),
      ],
      foundations: [[], [], [], []],
      free: [None, None, None, None],
    }
  }

  let winCheck = (game: game) => {
    game.piles->Array.every(pile => pile->Array.length == 0) &&
      game.free->Array.every(Option.isNone)
  }

  let removeDragFromGame = (game: game, dragPile: dragPile) => {
    let removeDragPile = x =>
      x->Array.filter(sCard => {
        !(dragPile->Array.some(dCard => sCard == dCard))
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

  let pileBaseRules = (i): staticSpace => {
    {
      droppedUpon: (game, dragPile) => {
        let dragPileBase = dragPile->Array.getUnsafe(0)
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
      autoProgress: false,
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
      applyMoveToOthers: move => {
        pile->Array.get(j + 1)->Option.mapOr((), x => move(Card(x)))
      },
      dragPile: () => {
        let dragPile = pile->Array.sliceToEnd(~start=j)
        if dragPile->dragPileValidation {
          Some(dragPile)
        } else {
          None
        }
      },
      autoProgress: () => {
        if isLast {
          Send([card])
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
    }
  }

  let foundationBaseRules = (i): staticSpace => {
    {
      autoProgress: true,
      droppedUpon: (game, dragPile) => {
        let justOne = dragPile->Array.length == 1
        let dragPileBase = dragPile->Array.getUnsafe(0)
        let noChildren = game.foundations->Array.getUnsafe(i)->Array.length == 0

        if noChildren && justOne && dragPileBase.rank == RA {
          Some({
            ...game,
            foundations: game.foundations->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
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
      applyMoveToOthers: _ => (),
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

        if justOne && dragPileBase.suit == card.suit && Card.rankIsBelow(card, dragPileBase) {
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
    }
  }

  let freeBaseRules = (i): staticSpace => {
    autoProgress: false,
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
  }

  let freeRules = (card, i): movableSpace => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: 1,
      },
      baseSpace: Free(i),
      applyMoveToOthers: _ => (),
      autoProgress: () => Send([card]),
      dragPile: () => Some([card]),
      droppedUpon: (_game, _dragPile) => None,
    }
  }

  let getRule: GameBase.getRule<game, space, dragPile> = (game: game, match: space) => {
    let result = ref(None)

    game.piles->Array.forEachWithIndex((pile, i) => {
      if Pile(i) == match {
        result := pileBaseRules(i)->Static->Some
      }

      pile->Array.forEachWithIndex((card, j) => {
        if Card(card) == match {
          result := pileRules(pile, card, i, j)->Movable->Some
        }
      })
    })

    game.foundations->Array.forEachWithIndex((foundation, i) => {
      if Foundation(i) == match {
        result := foundationBaseRules(i)->Static->Some
      }

      foundation->Array.forEachWithIndex((card, j) => {
        if Card(card) == match {
          result := foundationRules(game, card, i, j)->Movable->Some
        }
      })
    })

    game.free->Array.forEachWithIndex((card, i) => {
      if Free(i) == match {
        result := freeBaseRules(i)->Static->Some
      }

      card->Option.mapOr((), card => {
        if Card(card) == match {
          result := freeRules(card, i)->Movable->Some
        }
      })
    })

    result.contents
  }

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
    ) => {
      <React.Fragment>
        <div className="flex flex-row">
          <div className="flex flex-row gap-3">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Free(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Free(i)))}
                className=" border border-slate-200 bg-slate-100 rounded w-14 h-20"
              />
            })
            ->React.array}
          </div>
          <div className="flex flex-row gap-3 ml-10">
            {[[], [], [], []]
            ->Array.mapWithIndex((_, i) => {
              <div
                key={Free(i)->spaceToString}
                ref={ReactDOM.Ref.callbackDomRef(setRef(Foundation(i)))}
                className=" border border-slate-200 bg-slate-100 rounded w-14 h-20"
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
              className=" border border-slate-200 bg-slate-100  rounded w-14 h-20"
            />
          })
          ->React.array}
        </div>
      </React.Fragment>
    }
  }

  module AllCards = {
    @react.component
    let make = (~setRef, ~onMouseDown) => {
      <React.Fragment>
        {shuffledDeck
        ->Array.map(card => {
          <Card.Display
            card={card}
            key={Card(card)->spaceToString}
            id={Card(card)->spaceToString}
            cardRef={ReactDOM.Ref.callbackDomRef(setRef(Card(card)))}
            onMouseDown={onMouseDown}
          />
        })
        ->React.array}
      </React.Fragment>
    }
  }
}

module Game = GameBase.Create(GameRules)
