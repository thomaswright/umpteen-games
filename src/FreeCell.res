open Webapi.Dom
open Types
open Common

module GameRules = {
  let foundationOffset = 40 + 70 * 4

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

  type game = {
    piles: array<array<Card.card>>,
    foundations: array<array<Card.card>>,
    free: array<option<Card.card>>,
    gameEnded: bool,
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
      gameEnded: false,
    }
  }

  type autoProgress<'a> = Send('a) | Seek | DoNothing

  type compound = {
    locationAdjustment: pos,
    baseSpace: space,
    dragPile: unit => option<array<Card.card>>,
    autoProgress: unit => autoProgress<array<Card.card>>,
    droppedUpon: (game, array<Card.card>) => option<game>,
    droppedUponBase: (game, array<Card.card>) => option<game>,
    applyMoveToOthers: (space => unit) => unit,
  }

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

  let pileRules = (game, pile, card, i, j) => {
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
      droppedUponBase: (game, dragPile) => {
        let dragPileTop = dragPile->Array.getUnsafe(0)
        if dragPileTop.rank == RK {
          Some({
            ...game,
            piles: game.piles->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
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

  let foundationRules = (game, card, i, j) => {
    {
      locationAdjustment: {
        x: 0,
        y: 0,
        z: j + 1,
      },
      baseSpace: Pile(i),
      applyMoveToOthers: _ => (),
      dragPile: () => {
        if j == game.foundations->Array.length - 1 {
          Some([card])
        } else {
          None
        }
      },
      autoProgress: () => {
        Seek
      },
      droppedUponBase: (game, dragPile) => {
        let justOne = dragPile->Array.length == 1
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if justOne && dragPileBase.rank == RA {
          Some({
            ...game,
            foundations: game.foundations->ArrayAux.update(i, _ => dragPile),
          })
        } else {
          None
        }
      },
      droppedUpon: (game, dragPile) => {
        let justOne = dragPile->Array.length == 1
        let dragPileBase = dragPile->Array.getUnsafe(0)

        if justOne && dragPileBase.rank == card.rank && Card.rankIsAbove(card, dragPileBase) {
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

  let freeRules = (card, i) => {
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
      droppedUponBase: (game, dragPile) => {
        if dragPile->Array.length == 1 {
          Some({
            ...game,
            free: game.free->ArrayAux.update(i, _ => dragPile->Array.get(0)),
          })
        } else {
          None
        }
      },
    }
  }

  let compound = (game: game, match) => {
    let result = ref(None)
    game.piles->ArrayAux.forEach2((pile, card, i, j) => {
      if card == match {
        result := Some(pileRules(game, pile, card, i, j))
      }
    })

    game.foundations->ArrayAux.forEach2((_foundation, card, i, j) => {
      if card == match {
        result := Some(foundationRules(game, card, i, j))
      }
    })

    game.free->Array.forEachWithIndex((card, i) => {
      card->Option.mapOr((), card => {
        if card == match {
          result := Some(freeRules(card, i))
        }
      })
    })
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

// module Game = GameBase.GameBase(GameRules)
